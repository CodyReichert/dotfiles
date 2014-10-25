(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
        package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives )

;; chrome reload browser (<C-x><C-r>)
(defun chrome-reload() (interactive)
  (shell-command "chromix with localhost reloadWithoutCache"))
(define-key global-map "\C-x\C-r" 'chrome-reload)

;; tabs -> spaces
(setq-default indent-tabs-mode nil)

;; evil mode
(package-initialize)
(evil-mode 1)

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build) ;; build

(custom-set-variables
 '(haskell-font-lock-symbols t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save (not t))
 '(nil nil t)
 '(safe-local-variable-values (quote ((hamlet/basic-offset . 4) (haskell-process-use-ghci . t) (haskell-indent-spaces . 4)))))

;; bind f5 to reload DevelMain
(define-key haskell-mode-map [f5] 'haskell-process-reload-devel-main)

(eval-after-load 'hamlet-mode
  '(define-key hamlet-mode-map [f5] 'haskell-process-reload-devel-main))

(eval-after-load 'less-css-mode ;; for lucius files
  '(define-key css-mode-map [f5] 'haskell-process-reload-devel-main))
  
(eval-after-load 'js            ;; for julius files
  '(define-key js-mode-map [f5] 'haskell-process-reload-devel-main))
  
;; highlight parentheses in all buffers
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; smooth scrolling
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

;; org-mode settings
(require 'org)

(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)

(setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "SCHEDULED" "WAITING" "DONE")))

(setq org-todo-keyword-faces
        '(("TODO" . org-warning) ("IN-PROGRESS" . "yellow") ("SCHEDULED" . "white")
         ("WAITING" . "cyan") ("DONE" . "green")))

(require 'org-list)
(add-to-list 'org-checkbox-statistics-hook (function
					     ndk/checkbox-list-complete))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; activate modes for file extenions
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . less-css-mode)) ;; less-mode lucius files
(add-to-list 'auto-mode-alist '("\\.julius\\'" . js-mode))       ;; javascript-mode julius files

;; autocomplete everywhere
(global-auto-complete-mode t)
