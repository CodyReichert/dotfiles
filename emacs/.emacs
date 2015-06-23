;;; .emacs - Cody Reichert

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages and Repositories
;; On startup, initialize packages and repositories. Check for all of the
;; packages listed and if they don't exist, install them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-list '(tabulated-list auctex
                                    auto-complete
                                    circe
                                    coffee-mode
                                    dash
                                    emmet-mode
                                    epl
                                    evil
                                    evil-leader
                                    evil-numbers
                                    evil-org
                                    expand-region
                                    flycheck
                                    git-commit-mode
                                    git-rebase-mode
                                    ghci-completion
                                    goto-chg
                                    haskell-mode
                                    helm
                                    helm-projectile
                                    highlight-parentheses
                                    ido-vertical-mode
                                    js2-mode
                                    json-mode
                                    json-reformat
                                    json-snatcher
                                    jsx-mode
                                    jquery-doc
                                    key-chord
                                    latex-extra
                                    less-css-mode
                                    lisp-editing
                                    magit
                                    markdown-mode
                                    moz
                                    nodejs-repl
                                    org
                                    org-page
                                    php-mode
                                    phpunit
                                    pkg-info
                                    popup
                                    projectile
                                    recompile-on-save
                                    s
                                    shakespeare-mode
                                    smart-mode-line
                                    smex
                                    smooth-scroll
                                    switch-window
                                    undo-tree
                                    web-mode
                                    yaml-mode
                                    ztree
                                    ))

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


(add-to-list 'load-path "~/.emacs.d/scripts")
(add-to-list 'load-path "~/.emacs.d/es6-snippets")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings and Initializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-fringe-mode nil)

(setq-default indent-tabs-mode nil)

(setq initial-buffer-choice '(lambda () (eshell)))

(setq x-select-enable-clipboard t)

(global-set-key (kbd "C-x o") 'switch-window)

(global-set-key (kbd "C-c C-v") 'er/expand-region)

(menu-bar-mode -1)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)


(global-set-key (kbd "C-c C-r") 'rename-buffer)

(setq electric-pair-mode 1)


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

;; smex
(global-set-key (kbd "M-x") 'smex)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; electric pair mode
(setq electric-pair-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save (not t))
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   (quote
    ((hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; switch-window mode
(global-set-key (kbd "C-x o") 'switch-window)


;; expand-region mode
(global-set-key (kbd "C-c C-v") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)
(require 'org)

(setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "SCHEDULED" "WAITING" "DONE")))

(setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("IN-PROGRESS" . "yellow")
          ("SCHEDULED" . "white")
          ("WAITING" . "cyan")
          ("DONE" . "green")))

(setq org-tag-faces
        '(("FEATURE"  . (:foreground "green"))
          ("BUG"  . (:foreground "#ffffff" :background "#C00000" :underline t))
          ("DOCUMENTATION"  . (:foreground "gray"))
          ("RESEARCH"  . (:foreground "gray"))
          ("TEMPLATE"  . (:foreground "orange"))
          ("REFACTOR"  . (:foreground "red"))))

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
   (js . t)
   (perl . t)
   (haskell . t)
))

(setq org-log-done t)


(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "Javascript" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
        (org-edit-src-code)))


(add-hook 'org-mode-hook '(lambda ()
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ))


(setq org-src-fontify-natively t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               haskell-mode                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            shakespeare-mode                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f7] 'haskell-process-cabal-build))

;; (eval-after-load 'shakespeare-mode
;;   '(define-key shakespeare-mode-map [f5] 'haskell-process-reload-devel-main))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                web-mode                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;;web-mode for js/jsx

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                          '(json-jsonlist)))

(defun web-mode-indent-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-indent-hook)
(add-hook 'web-mode-hook  'emmet-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-attribute  'magit-diff-foreground "white")
     (set-face-attribute  'magit-diff-background "black")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black")
       (set-face-foreground 'magit-item-highlight "white"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Browsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chromium reload browser (<C-x C-r>)
(defun chrome-reload()
  "Reload Chrome/Chromium from emacs. Requires the npm package 'chromix'"
  (interactive)
  (shell-command "chromix with localhost reloadWithoutCache")
  (shell-command "chromix with file reloadWithoutCache"))
(define-key global-map "\C-x\C-r" 'chrome-reload)


;; Firefox reload on save
(require 'moz)
(define-minor-mode moz-reload-on-save-mode
  "Moz Reload On Save Minor Mode. When activated in a buffer, firefox will
   reload on save"
  nil " Reload" nil
  (if moz-reload-on-save-mode
      (add-hook 'after-save-hook 'moz-firefox-reload nil t)
    (remove-hook 'after-save-hook 'moz-firefox-reload t)))


(defun moz-firefox-reload () (interactive)
  (comint-send-string (inferior-moz-process) "BrowserReload();"))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Complete-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(setq ac-auto-show-menu t)

(set-default 'ac-sources
             '(
               ac-source-filename
               ;; ac-source-files-in-current-dir
               ac-source-imenu
               ac-source-words-in-buffer
               ac-source-words-in-all-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-yasnippet
               ac-source-dictionary
               ;; ac-source-gtags
               ;; ac-source-ispell
               ))

(require 'jquery-doc)
(add-hook 'web-mode-hook 'jquery-doc-setup)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Mode Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use setq-default to set it for /all/ modes
(require 'smart-mode-line)

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

(setq sml/theme nil)
(sml/setup)
(set-face-background 'modeline-inactive nil)
(set-face-background 'mode-line nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Utility Functions                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun latex-compile(x)
  "Compile TeX files to pdf and reload chromium to view changes"
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


(defun npm-command (command)
  "start run a script from an npm package.json (it works, but needs some work)"
  (interactive
   (let* ((default-directory (locate-dominating-file default-directory "package.json"))
          (command (concat "npm run "))
          (compilation-buffer-name-function (lambda (mode) (concat "npm-run-" command))))
     (list (compilation-read-command command))))
  (compile command))


(defun xah-escape-quotes ()
    "Replace 「\"」 by 「\\\"」 in current line or text selection.
     That is, add 1 backslash in front of double quote (Unicode codepoint 34).
     See also: `xah-unescape-quotes'
     Version 2015-01-24
     URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
    "
    (interactive)
    (let (p1 p2)
      (if (use-region-p)
          (progn (setq p1 (region-beginning))
                 (setq p2 (region-end)))
        (progn (setq p1 (line-beginning-position))
               (setq p2 (line-end-position))))
      (save-excursion
        (save-restriction
          (narrow-to-region p1 p2)
          (goto-char (point-min))
          (while (search-forward "\"" nil t)
                      (replace-match "\\\"" 'FIXEDCASE 'LITERAL))))))

(define-key global-map "\C-c\C-y" 'xah-escape-quotes)


;; open file as sudo with tramp
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   Compilation Buffer                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer"
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-scroll-output t)


(autoload 'compilation-always-kill-mode "compilation-always-kill" nil t)
(compilation-always-kill-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Yasnippets                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(require 'react-snippets)
(require 'es6-snippets)

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Projectile                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(projectile-global-mode)

(global-set-key (kbd "C-c T")       'projectile-regenerate-tags)
(global-set-key (kbd "C-c t")       'projectile-test-project)
(global-set-key (kbd "C-c g")       'projectile-grep)
(global-set-key (kbd "C-c i")       'projectile-ibuffer)
(global-set-key (kbd "C-x C-d")     'projectile-switch-project)
(global-set-key [f5]                'projectile-compile-project)


(defun projectile-or-ido-find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'projectile-find-file
     'ido-find-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   Circe (IRC Client)                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/.private.el")
(setq circe-network-options
      '(("Freenode"
         :nick "CodyReichert"
         :channels ("#emacs" "#archlinux" "#haskell" "#reactjs")
         :nickserv-password ,circe-pass)))

(defun irc ()
  "Connect to IRC with circe"
  (interactive)
  (circe "Freenode"))

(load-file "~/.emacs.d/circe-notifications/circe-notifications.el")
(autoload 'enable-circe-notifications "circe-notifications" nil t)

(eval-after-load "circe-notifications"
  '(setq circe-notifications-watch-nicks
         '("CodyReichert")
         circe-notifications-backend "notify-send"))

(setq circe-notifications-check-window-focus t)

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Org-Page (Blog)                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-page)
(setq op/repository-directory "/home/cody/workspace/org/blog")
(setq op/site-domain "http://codyreichert.github.io")
(setq op/personal-github-link "https://github.com/CodyReichert")
(setq op/site-main-title "The One True Blog")
(setq op/site-sub-title "Emacs, programming, and whatever else comes to mind")
(setq op/personal-disqus-shortname "theonetrueblog")
(setq op/theme 'mdo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         eshell                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)
(require 'helm-mpd)


(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     nil
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)


(defun projectile-or-helm-find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'helm-projectile-find-file
     'helm-find-files)))


(helm-mode 1)
(helm-projectile-on)


(global-set-key (kbd "C-x C-f") 'projectile-or-helm-find-file)
(global-set-key (kbd "C-x f")   'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)


;;; .emacs ends here
