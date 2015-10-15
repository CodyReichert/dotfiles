;;; inbox.el --- display inbox status information  -*- coding: utf-8 -*-

;; Copyright (C) 2014 Sergei Glushchenko.

;; Author: Sergei Glushchenko <gl.sergei@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script has been modified to simply return a string, which can be used
;; anywhere instead of just the modeline.

;;; Code:


(defgroup inbox nil
  "Display inbox status information."
  :prefix "inbox-"
  :group 'hardware)

(defcustom inbox-update-interval 60
  "Inbox update interval."
  :type 'integer
  :group 'inbox)

(defvar inbox-update-timer nil
  "Interval timer object.")

(defcustom inbox-count-command
  "echo -n $( mu find date:1w..now maildir:/simplyrets/INBOX flag:unread 2>/dev/null | wc -l )"
  "Command to retrieve count of emails in Inbox."
  :type 'string
  :group 'inbox)

(defvar inbox-mode-line-string nil
  "String to display in the mode line.")
(put 'inbox-mode-line-string 'risky-local-variable t)

(define-minor-mode display-inbox-mode
  "Toggle inbox status display in mode line (Display Inbox mode).
With a prefix argument ARG, enable Display Inbox mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'inbox
  (setq inbox-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and inbox-update-timer (cancel-timer inbox-update-timer))

  (if (not display-inbox-mode)
      (setq global-mode-string
            (delq 'inbox-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'inbox-mode-line-string t)
    (setq inbox-update-timer (run-at-time nil inbox-update-interval
                                          'inbox-update-handler))
    (inbox-update)))

(defvar sml-inbox-count-string "")

(defun inbox-update ()
  (interactive)
  (setq sml-inbox-count-string
        (let ((unread (shell-command-to-string inbox-count-command)))
        (format "%s" unread))))

(defun inbox-update-handler ()
  (inbox-update)
  (sit-for 0))

(provide 'inbox)

;;; inbox.el ends here
