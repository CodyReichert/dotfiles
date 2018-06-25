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

(defun use-eslint-from-node-modules ()
  "Find the eslint binary local to the current file to use the correct configuration, plugins, etc."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook 'use-eslint-from-node-modules)

(provide 'init-web)
;;; init-web.el ends here
