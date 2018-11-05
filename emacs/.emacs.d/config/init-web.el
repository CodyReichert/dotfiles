;;; init-web.el --- Web Mode

;;; Commentary:
;;; web-mode settings

;;; Code:
(add-to-list 'load-path "/home/cody/workspace/repos/flowmacs")

(require 'web-mode)
(require 'flycheck)
(require 'add-node-modules-path)
(require 'prettier-js)
(require 'flowmacs)

;;; Web-mode setup

(add-to-list 'auto-mode-alist '("\\.eslintrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-auto-quote-style nil)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (add-node-modules-path)
  (flowmacs-mode)
  (prettier-js-mode))

(defun my/local-flow-bin ()
  "Tell flowmacs to use flow-bin from local node_modules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root (expand-file-name
                          "node_modules/.bin/flow"
                          root))))
    (when (and flow (file-executable-p flow))
      (setq-local flowmacs/+flow+ flow))))

(add-hook 'flowmacs-mode-hook 'my/local-flow-bin)
(add-hook 'web-mode-hook 'web-mode-init-hook)
(add-hook 'web-mode-hook 'emmet-mode)

;;; Flycheck setup

(add-hook 'web-mode-hook #'flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(provide 'init-web)
;;; init-web.el ends here
