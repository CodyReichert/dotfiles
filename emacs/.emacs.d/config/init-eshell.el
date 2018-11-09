;;; init-eshell.el --- Eshell settings

;;; Commentary:
;;; eshell settings

;;; Code:
(require 'eshell)
(require 'em-smart)

(add-hook 'eshell-preoutput-filter-function 'ansi-color-filter-apply)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          '(lambda ()
             (set-face-attribute 'eshell-prompt nil :foreground "#228b22")
             (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input)
             (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)))

(defun eshell/clear ()
  "Clear the eshell buffer, similar to bash's `clear'."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

(provide 'init-eshell)
;;; init-eshell.el ends here
