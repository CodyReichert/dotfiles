;;; init-web.el --- Web Mode

;;; Commentary:
;;; web-mode settings

;;; Code:
(add-to-list 'load-path "~/.emacs.d/flowmacs")

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
  (setq web-mode-enable-current-column-highlight t)
  (web-mode-toggle-current-element-highlight)
  (electric-pair-local-mode)
  (electric-indent-local-mode)
  (electric-layout-mode)
  (electric-pair-mode)
  (add-node-modules-path)
  (flowmacs-mode)
  (prettier-js-mode))

(load-file "~/.emacs.d/webpack-dev-server.el/webpack-dev-server.el")
(setq webpack-dev-server-command
      "make -C ~/workspace/assertible/assertible frontend-dev")


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

(defun setup-pragmata-ligatures ()
  (setq prettify-symbols-alist
        (append prettify-symbols-alist
         '(("!!"   . ?↯)
           ("!="   . ?≠)
           ("!=="  . ?≢)
           ("<="   . ?≤)
           (">="   . ?≥)
           ("..."  . ?…)
           ("=>"   . ?⇒)
           ;; (" * "  . ?×)
           ("||"   . ?∨)
           ("&&"   . ?∧)
           ("==="  . ?≡)))))

(defun refresh-pretty ()
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

  ;; Hooks for modes in which to install the Pragmata ligatures
(mapc (lambda (hook)
        (add-hook hook (lambda () (setup-pragmata-ligatures) (refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook))

(global-prettify-symbols-mode +1)


(provide 'init-web)
;;; init-web.el ends here
