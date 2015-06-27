;;; init-haskell.el --- Haskell Mode

;;; Commentary:
;;; haskell-mode settings

;;; Code:
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)

;; (eval-after-load 'haskell-mode
;;   '(define-key haskell-indentation-mode-map (kbd "<backtab>") nil))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-interactive-popup-errors 'nil)

(setq haskell-process-type 'cabal-repl)

(setq haskell-process-use 'cabal-repl)

;;(define-key haskell-mode-map [f5] 'haskell-process-reload-devel-main)

;;(eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f7] 'haskell-process-cabal-build))

;; (eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f5] 'haskell-process-reload-devel-main))

(provide 'init-haskell)
;;; init-haskell.el ends here
