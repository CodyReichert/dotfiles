;;; init-haskell.el --- Haskell Mode

;;; Commentary:
;;; haskell-mode settings

;;; Code:
(require 'haskell-mode)

(setq haskell-process-reload-with-fbytecode nil
      haskell-process-use-presentation-mode t
      haskell-process-type 'stack-ghci)

(require 'flycheck-stack)

(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)
            (progn
              (haskell-indentation-mode)
              (turn-on-haskell-doc)
              (turn-on-haskell-decl-scan)
              (interactive-haskell-mode)
              (define-key haskell-mode-map (kbd "C-c C-;") 'haskell-process-load-file)
              (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-reload)
              (define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports-go)
              (define-key haskell-mode-map (kbd "C-c I") 'haskell-navigate-imports-return)
              (flycheck-select-checker 'haskell-stack-ghc)
              )))

(add-hook 'haskell-interactive-mode-hook
          (lambda ()
            (setq compilation-skip-threshold 2)))

(provide 'init-haskell)
;;; init-haskell.el ends here
