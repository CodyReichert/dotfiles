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

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

(provide 'init-eshell)
;;; init-eshell.el ends here
