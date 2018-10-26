;;; init-haskell.el --- Haskell Mode

;;; Commentary:
;;; haskell-mode settings

;;; Code:
;; haskell

(require 'haskell-mode)

(setq haskell-process-reload-with-fbytecode nil
      haskell-process-use-presentation-mode t
      haskell-process-type 'stack-ghci
      haskell-indent-spaces 4)

;; https://gist.github.com/989ad8be92f68682abff
(defun haskell-run-function-under-cursor ()
  "Send the word-at-point as a function to GHCi process."
  (interactive)
  (haskell-process-send-string
	    (haskell-interactive-process)
	    (format "%s" (word-at-point))))

(add-to-list 'haskell-font-lock-quasi-quote-modes '("yamlQQ" . yaml-mode))
(add-to-list 'haskell-font-lock-quasi-quote-modes '("js"     . web-mode))
(add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-O0")
(add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-fshow-loaded-modules")

(add-to-list 'auto-mode-alist '("\\.lhs\\'"   . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hss\\'"   . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cpphs\\'" . haskell-c-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'"   . hakell-c-mode))

(add-to-list 'interpreter-mode-alist '("stack"      . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

;; This is a flycheck plugin for stack I wrote before support wass added
;; to flycheck-haskell. It doesn't work quite as well as flycheck's, as
;; it's not based around ghci/runghc w/ optimized ghc flags.
;;
;; (add-to-list 'load-path (substitute-in-file-name "$HOME/.emacs.d/flycheck-haskell-stack/"))
;; (require 'flycheck-haskell-stack)
;; (flycheck-select-checker 'haskell-stack)

(require 'flycheck-haskell)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(add-hook 'haskell-mode-hook
	  (lambda ()
            (turn-on-haskell-indent)
            (haskell-doc-mode)
            (haskell-decl-scan-mode)

            (setq haskell-process-args-stack-ghci
                     '("--no-load"
                       "--ghci-options=-O0"
                       "--ghci-options=-ferror-spans"
                       "--ghci-options=-fshow-loaded-modules"))

            (define-key evil-motion-state-map "f" 'haskell-mode-jump-to-def-or-tag)
            (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
            (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)

            (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
            (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
            (define-key haskell-mode-map (kbd "C-c C-;") 'haskell-process-load-file)
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-reload)
            (define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports-go)
            (define-key haskell-mode-map (kbd "C-c I") 'haskell-navigate-imports-return)
            (define-key haskell-mode-map (kbd "C-c C-j") 'haskell-run-function-under-cursor)
            ))

(provide 'init-haskell)
;;; init-haskell.el ends here
