;;; sys-idle.el --- Get system-wide idle time  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; Created:  2026-01-05
;; Keywords: tools
;; URL:      https://github.com/meedstrom/sys-idle
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Call `sys-idle-seconds' to get the number of seconds since last user
;; activity on the computer.

;; Supports (for now):

;; - Mac OS
;; - Windows
;; - Wayland (GNOME)
;; - Wayland (KDE)
;; - X11

;;; Code:

(declare-function dbus-list-activatable-names "dbus" (&optional bus))
(declare-function dbus-call-method "dbus" (bus service path interface method &rest args))
(declare-function dbus-get-property "dbus" (bus service path interface property))
(require 'seq)

(defun sys-idle-seconds--poll-mac ()
  "Copy of `org-mac-idle-seconds'."
  (string-to-number
   (shell-command-to-string
    "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

(defvar sys-idle--x11-program nil)
(defun sys-idle-seconds--poll-x11 ()
  "Like `org-x11-idle-seconds'."
  (unless sys-idle--x11-program
    (error "sys-idle-seconds-x11: Install x11idle or xprintidle"))
  (with-temp-buffer
    (if (eq 0 (call-process sys-idle--x11-program nil t))
        (/ (string-to-number (buffer-string)) 1000)
      (error "sys-idle-seconds-x11: %s failed: %s"
             sys-idle--x11-program
             (buffer-string)))))

(defvar sys-idle--dbus-session-path nil
  "Copy of `org-logind-dbus-session-path'.")

(defun sys-idle-seconds--poll-elogind ()
  "Copy of `org-logind-user-idle-seconds'."
  (unless (fboundp 'dbus-get-property)
    (require 'dbus))
  (- (float-time)
     (/ (dbus-get-property :system
                           "org.freedesktop.login1"
                           sys-idle--dbus-session-path
                           "org.freedesktop.login1.Session"
                           "IdleSinceHint")
        1e6)))

;; https://unix.stackexchange.com/questions/396911/how-can-i-tell-if-a-user-is-idle-in-wayland
(defun sys-idle-seconds--poll-gnome ()
  "Check Mutter's idea of idle time, even on Wayland."
  (let* ((output (with-temp-buffer
                   (call-process "dbus-send" nil (current-buffer) nil
                                 "--print-reply"
                                 "--dest=org.gnome.Mutter.IdleMonitor"
                                 "/org/gnome/Mutter/IdleMonitor/Core"
                                 "org.gnome.Mutter.IdleMonitor.GetIdletime")
                   (buffer-string)))
         (idle-ms (if (string-match (rx space (+ digit) eol) output)
                      (string-to-number (match-string 0 output))
                    (error "Function `sys-idle-seconds--poll-gnome' not working"))))
    (/ idle-ms 1000.0)))

;; https://github.com/swaywm/swayidle/issues/147
;; https://github.com/swaywm/swayidle/issues/181
;; https://github.com/marvin1099/wayidletool
(defvar sys-idle--swayidle-process nil)
(defun sys-idle-seconds--poll-swayidle ()
  "Check idle on compositors supporting the ext-idle-notify protocol.
This includes KDE Plasma and Sway.
Returns 0 if invoked during the first 9 seconds."
  (when (not (process-live-p sys-idle--swayidle-process))
    (setq sys-idle--swayidle-process
          (start-process-shell-command
           "swayidle"
           " *sys-idle:swayidle*"
           "swayidle timeout 9 'touch /tmp/sys-idle' resume 'rm /tmp/sys-idle'"))
    (set-process-query-on-exit-flag sys-idle--swayidle-process nil))
  (let ((attr (file-attributes "/tmp/sys-idle")))
    (if attr
        (+ 9 (time-to-seconds
              (time-since (file-attribute-modification-time attr))))
      0)))

(defvar sys-idle-seconds-function nil
  "Function invoked by `sys-idle-seconds', must return a number.
You can manually set this to `sys-idle-seconds-fallback'
if you know what you are doing.")

(defun sys-idle-seconds ()
  "Return the number of seconds the system has been idle.

Unlike `current-idle-time', the result is intended to be correct even
while Emacs is not in focus.  However, that assumes that the current
value of `sys-idle-seconds-function' does this correctly.

Always returns a number."
  (unless sys-idle-seconds-function
    ;; Guess which function is most appropriate on this system.
    (setq sys-idle-seconds-function
          (or (and (eq system-type 'darwin)
                   #'sys-idle-seconds--poll-mac)
              (let ((DESKTOP_SESSION (getenv "DESKTOP_SESSION")))
                (and DESKTOP_SESSION
                     (not (string-search "xorg" DESKTOP_SESSION))
                     (if (string-match-p (rx (or "gnome" "ubuntu")) DESKTOP_SESSION)
                         #'sys-idle-seconds--poll-gnome
                       (and (string-match-p (rx "plasma") DESKTOP_SESSION) ;; KDE
                            (if (executable-find "swayidle")
                                #'sys-idle-seconds--poll-swayidle
                              (error "sys-idle-seconds: Install swayidle"))))))
              (and (setq sys-idle--dbus-session-path
                         (when (and (boundp 'dbus-runtime-version)
                                    (require 'dbus nil t)
                                    (member "org.freedesktop.login1" (dbus-list-activatable-names)))
                           (ignore-errors
                             (dbus-call-method
                              :system "org.freedesktop.login1"
                              "/org/freedesktop/login1"
                              "org.freedesktop.login1.Manager"
                              "GetSessionByPID" (emacs-pid)))))
                   #'sys-idle-seconds--poll-elogind)
              ;; NOTE: This condition is also true under XWayland, so it must come
              ;; after all other checks for Wayland compositors if we want it to be
              ;; invoked under "true" X only.
              (and (eq window-system 'x)
                   (or (setq sys-idle--x11-program
                             (seq-find #'executable-find '("x11idle" "xprintidle")))
                       (error "sys-idle-seconds: Install x11idle or xprintidle"))
                   #'sys-idle-seconds--poll-x11)
              (error "sys-idle-seconds: Could not get idle time on this system"))))
  (or (funcall sys-idle-seconds-function)
      (error "Function at sys-idle-seconds-function did not return a number")))

(defun sys-idle-seconds-fallback ()
  "Copy of `org-emacs-idle-seconds'.

Note that this can keep returning a higher number even while user is
actively using the computer, because it technically returns the amount
of time since Emacs was last in focus."
  (let ((idle (current-idle-time)))
    (if idle (float-time idle) 0)))

(provide 'sys-idle)

;;; sys-idle.el ends here
