;; initialize packages and repositories
(setq package-list '(tabulated-list auto-complete coffee-mode expand-region smart-mode-line
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
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save (not t))
 '(menu-bar-mode nil)
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
  
;; (eval-after-load 'js            ;; for julius files
;;   '(define-key js2-mode-map [f5] 'haskell-process-reload-devel-main))

(eval-after-load 'js            ;; for julius files
  '(define-key js2-mode-map [f5] 'haskell-process-reload-devel-main))
  
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
;; (add-to-list 'auto-mode-alist '("\\.julius\\'" . js-mode))       ;; js-mode julius files
(add-to-list 'auto-mode-alist '("\\.julius\\'" . js2-mode))       ;; js2-mode julius files

;; autocomplete everywhere
(global-auto-complete-mode t)
(auto-complete-mode 1)
(global-auto-complete-mode 1)

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

(eval-after-load 'magit
     (set-face-attribute 'magit-item-highlight
                         nil
                         :background "#505050"
                         :foreground "white"))

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; disable menu bar
(menu-bar-mode -1)

;; use setq-default to set it for /all/ modes
(setq mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; uncomment next line for minor modes in mode-line
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
))

(sml/setup)
(set-face-background 'mode-line "black")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
