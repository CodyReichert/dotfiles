;;; init-eshell.el --- Eshell settings

;;; Commentary:
;;; eshell settings

;;; Code:
(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(provide 'init-eshell)
;;; init-eshell.el ends here
