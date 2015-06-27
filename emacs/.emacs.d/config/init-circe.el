;;; init-circe.el --- Circe settings

;;; Commentary:
;;; circe settings

;;; Code:
(require 'circe)

(load-file "~/.emacs.d/.private.el")
(load-file "~/.emacs.d/circe-notifications/circe-notifications.el")
(autoload 'enable-circe-notifications "circe-notifications" nil t)

(setq circe-network-options
      '(("Freenode"
         :nick "CodyReichert"
         :channels ("#emacs" "#archlinux" "#haskell" "#reactjs")
         :nickserv-password ,circe-pass)))

(defun irc ()
  "Connect to IRC with circe."
  (interactive)
  (circe "Freenode"))


(eval-after-load "circe-notifications"
  '(setq circe-notifications-watch-nicks
         '("CodyReichert")
         circe-notifications-backend "notify-send"))

(setq circe-notifications-check-window-focus t)
(setq circe-reduce-lurker-spam t)

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)
(add-hook 'circe-server-connected-hook 'enable-circe-color-nicks)

(provide 'init-circe)
;;; init-circe.el ends here
