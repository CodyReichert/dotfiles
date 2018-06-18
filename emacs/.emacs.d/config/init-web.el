;;; init-web.el --- Web Mode

;;; Commentary:
;;; web-mode settings

;;; Code:

(require 'web-mode)
(require 'flycheck)

;;; Web-mode setup

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-indent-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-indent-hook)
(add-hook 'web-mode-hook  'emmet-mode)

;;; Flycheck setup

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'init-web)
;;; init-web.el ends here
