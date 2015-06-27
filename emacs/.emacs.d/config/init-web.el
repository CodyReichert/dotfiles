;;; init-web.el --- Web Mode

;;; Commentary:
;;; web-mode settings

;;; Code:
(require 'web-mode)
(require 'flycheck)


(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;;web-mode for js/jsx

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                          '(json-jsonlist)))

(defun web-mode-indent-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-indent-hook)
(add-hook 'web-mode-hook  'emmet-mode)


(provide 'init-web)
;;; init-web.el ends here
