;;; init-haskell.el --- Haskell Mode

;;; Commentary:
;;; haskell-mode settings

;;; Code:
(require 'haskell-mode)
(require 'flycheck)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)

;; (eval-after-load 'haskell-mode
;;   '(define-key haskell-indentation-mode-map (kbd "<backtab>") nil))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-interactive-popup-errors 'nil)

(setq haskell-process-type 'stack-ghci)

(setq haskell-process-use 'cabal-repl)

;;(define-key haskell-mode-map [f5] 'haskell-process-reload-devel-main)

;;(eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f7] 'haskell-process-cabal-build))

;; (eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f5] 'haskell-process-reload-devel-main))


(flycheck-define-checker haskell-stack
  "A Haskell syntax and type checker using ghc."
  :command ("stack" "ghc" "--" "-Wall" "-fno-code" "-XTemplateHaskell"
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`literate-haskell-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode literate-haskell-mode)
  :next-checkers ((warning . haskell-hlint)))


(defun haskell-mode-setup-hook ()
  "Disable the `haskell-ghc' check."
  (interactive)
  (progn
    (turn-on-haskell-indent) ; haskell stuff
    (turn-on-haskell-doc)
    (turn-on-haskell-decl-scan)
    (interactive-haskell-mode)
    (flycheck-select-checker 'haskell-stack)))


(add-hook 'haskell-mode-hook 'haskell-mode-setup-hook)


(provide 'init-haskell)
;;; init-haskell.el ends here
