;; initialize packages and repositories
(setq package-list '(tabulated-list auto-complete coffee-mode expand-region
                                    evil-leader evil-numbers evil-org evil flycheck dash 
                                    ghci-completion goto-chg hamlet-mode haskell-mode org
                                    highlight-parentheses js2-mode js3-mode json-mode json-reformat
                                    json-snatcher latex-extra auctex less-css-mode lisp-editing magit
                                    git-rebase-mode git-commit-mode markdown-mode nodejs-repl pkg-info epl
                                    popup s smooth-scroll tangotango-theme undo-tree yaml-mode))
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; chrome reload browser (<C-x><C-r>)
(defun chrome-reload() (interactive)
  (shell-command "chromix with localhost reloadWithoutCache")
  (shell-command "chromix with file reloadWithoutCache"))
(define-key global-map "\C-x\C-r" 'chrome-reload)

(defun latex-compile(x)
  (interactive "File: ")
  (setq filename (concat x))
  (setq command "pdflatex ")
  (setq texCompile (concat command filename))
  (shell-command texCompile)
  (shell-command "chromix with file reloadWithoutCache")
  (message "success"))
(define-key global-map "\C-c\C-w" 'latex-compile)

(eval-after-load 'tex-mode
  '(define-key tex-mode-map [f5] 'latex-compile))

;; tabs -> spaces
(setq-default indent-tabs-mode nil)

;; evil mode
(evil-mode 1)

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build) ;; build

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save (not t))
 '(safe-local-variable-values (quote ((hamlet/basic-offset . 4) (haskell-process-use-ghci . t) (haskell-indent-spaces . 4)))))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-interactive-popup-errors 'nil)

(setq haskell-process-type 'cabal-repl)
(setq haskell-process-use 'cabal-repl)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

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

;; magit
(define-key global-map [f6] 'magit-status)

(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-diff-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-define-key 'normal magit-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-log-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-diff-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
