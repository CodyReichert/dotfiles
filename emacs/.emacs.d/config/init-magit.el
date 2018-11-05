;;; init-magit.el --- Magit settings

;;; Commentary:
;;; Magit settings

;;; Code:
(require 'magit)

(setq magit-last-seen-setup-instructions "2.1.0")

(define-key global-map [f6] 'magit-status)
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'insert)
(evil-set-initial-state 'magit-diff-mode 'insert)

(add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
(add-to-list 'evil-emacs-state-modes 'magit-popup-sequence-mode)

(setq magit-restore-window-configuration t) ; that's the default actually
(setq magit-status-buffer-switch-function
      (lambda (buffer) ; there might already be an Emacs function which does this
        (pop-to-buffer buffer)
        (delete-other-windows)))

(provide 'init-magit)
;;; init-magit.el ends here
