;;; init-ido.el --- Mostly aesthetic settings

;;; Commentary:
;;; ido-mode settings

;;; Code:
(require 'ido)
(require 'ido-vertical-mode)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)

(provide 'init-ido)
;;; init-ido.el ends here
