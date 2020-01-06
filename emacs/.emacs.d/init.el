;;; init.el --- Emacs init file
;;  Author: Cody Reichert
;;; Commentary:
;;  Personal Emacs configuration
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar cody/gc-cons-threshold 20000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold cody/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold (* cody/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold cody/gc-cons-threshold)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; Settings without corresponding packages

(use-package emacs
  :preface
  (defvar cody/indent-width 2)
  :config
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq show-trailing-whitespace t)
  (add-hook 'after-save-hook 'delete-trailing-whitespace)
  (setq user-full-name "Cody Reichert"
        frame-title-format '("Emacs")
        ring-bell-function 'ignore
        default-directory "~/"
        frame-resize-pixelwise t
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        load-prefer-newer t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default line-spacing 3
                indent-tabs-mode nil
                tab-width cody/indent-width))

;;; Built-in packages

(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file "~/.emacs.d/to-be-dumped.el"))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package simple
  :ensure nil
  :config (column-number-mode +1))

(use-package "window"
  :ensure nil
  :preface
  (defun cody/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun cody/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") #'cody/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'cody/split-and-follow-vertically))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        make-backup-files nil))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package eldoc
  :ensure nil
  :diminish
  :hook (prog-mode . eldoc-mode)
  :config (setq eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-mode)
  :config (setq js-indent-level cody/indent-width))

(use-package xref
  :ensure nil
  :config
  (define-key prog-mode-map (kbd "s-b") #'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") #'xref-pop-marker-stack))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset cody/indent-width)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

(use-package prolog
  :ensure nil
  :mode (("\\.pl\\'" . prolog-mode))
  :config (setq prolog-indent-width cody/indent-width))

(use-package python
  :ensure nil
  :config (setq python-indent-offset cody/indent-width))

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  (blink-cursor-mode -1)
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro-13:weight=regular" t t)))

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function #'split-window-horizontally))

(use-package faces
  :ensure nil
  :preface
  (defun cody/disable-bold-and-fringe-bg-face-globally ()
    "Disable bold face and fringe background in Emacs."
    (interactive)
    (set-face-attribute 'fringe nil :background nil)
    (mapc #'(lambda (face)
              (when (eq (face-attribute face :weight) 'bold)
                (set-face-attribute face nil :weight 'normal))) (face-list)))
  :config (add-hook 'after-init-hook #'cody/disable-bold-and-fringe-bg-face-globally))

(use-package flyspell
  :ensure nil
  :diminish
  :config (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :bind ("s-j" . global-display-line-numbers-mode))

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package saveplace :config (save-place-mode +1))

(use-package recentf :config (recentf-mode +1))

;;; Third-party Packages

;; GUI enhancements

(use-package doom-themes
  :custom-face (cursor ((t (:background "#eeaf2c"))))
  :config (load-theme 'doom-dracula t))

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Dangerously powerful"
        dashboard-items nil
        dashboard-set-footer nil))

(use-package smart-mode-line-atom-one-dark-theme)

(use-package smart-mode-line
  :config
  (when (member "Menlo" (font-family-list))
    (progn
      (set-face-attribute 'mode-line nil :height 120 :font "Menlo")
      (set-face-attribute 'mode-line-inactive nil :height 120 :font "Menlo")))
  (setq sml/no-confirm-load-theme t
        sml/theme 'atom-one-dark)
  (sml/setup))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-close-button " × ")
  (when (member "Arial" (font-family-list))
      (centaur-tabs-change-fonts "Arial" 130))
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :diminish
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character 9615) ; left-align vertical bar
  (setq highlight-indent-guides-auto-character-face-perc 20))

(use-package highlight-symbol
  :diminish
  :hook (prog-mode . highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Vi keybindings

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width cody/indent-width)
  :hook (after-init . evil-mode)
  :preface
  (defun cody/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'cody/save-and-kill-this-buffer)
  (use-package evil-commentary
    :after evil
    :diminish
    :config (evil-commentary-mode +1)))

(use-package evil-magit)

;; Git integration

(use-package magit
  :bind ([f7] . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (evil-set-initial-state 'magit-mode 'normal)
  (evil-set-initial-state 'magit-log-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert)
  (add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-popup-sequence-mode)
  (setq magit-restore-window-configuration t) ; that's the default actually
  (setq magit-status-buffer-switch-function
        (lambda (buffer) ; there might already be an Emacs function which does this
          (pop-to-buffer buffer)
          (delete-other-windows))))

(use-package diff-hl
  :custom-face
  (diff-hl-insert ((t (:foreground "#55bb55" :background nil))))
  (diff-hl-delete ((t (:foreground "#ff6666" :background nil))))
  (diff-hl-change ((t (:foreground "#99bbdd" :background nil))))
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;; Searching/sorting enhancements & project management

(use-package flx)

(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
  (setq counsel-rg-base-command "rg --vimgrep %s"))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-display-style nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil))

(use-package swiper
  :after ivy
  :custom-face (swiper-line-face ((t (:foreground "#ffffff" :background "#60648E"))))
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package ivy-posframe
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20)))
  (if (member "Menlo" (font-family-list))
      (setq ivy-posframe-parameters '((internal-border-width . 10) (font . "Menlo")))
    ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode +1))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file) ; counsel
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep) ; counsel
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-completion-system 'ivy))

;; (use-package helm-git-grep
;;   :ensure helm)

(use-package ace-window
  :ensure t)

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (setq evil-leader/in-all-states t)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key "j" 'Control-X-prefix)
  (evil-leader/set-key "f" 'projectile-find-file)
  (evil-leader/set-key "w" 'save-buffer)
  (evil-leader/set-key "k" 'kill-this-buffer)
  (evil-leader/set-key "b" 'ivy-switch-buffer)
  (evil-leader/set-key "v" 'er/expand-region)

  ;; flow-mode.el provided these functions
  (evil-leader/set-key "j \\" 'flowmacs/flow-pretty-status)
  (evil-leader/set-key "j RET" 'flowmacs/flow-status)
  (evil-leader/set-key "j d" 'flowmacs/jump-to-def)
  (evil-leader/set-key "j r" 'flowmacs/find-refs)
  (evil-leader/set-key "j s" 'flowmacs/suggest-types)
  (evil-leader/set-key "j t" 'flowmacs/type-at-pos)

  ;; Window switching
  (evil-leader/set-key "p" 'ace-window)
  (evil-leader/set-key "o" 'aw-flip-window)
  (evil-leader/set-key "i" 'ace-swap-window)

  ;; web-mode keys
  (evil-leader/set-key "r w" 'web-mode)
  (evil-leader/set-key "a k" 'web-mode-attribute-kill)
  (evil-leader/set-key "e w" 'web-mode-element-wrap)
  (evil-leader/set-key "e r" 'web-mode-element-rename)
  (evil-leader/set-key "e v" 'web-mode-element-vanish)
  (evil-leader/set-key "e n" 'flycheck-next-error)
  (evil-leader/set-key "e p" 'flycheck-previous-error))

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-reload-with-fbytecode nil
        haskell-process-use-presentation-mode t
        haskell-process-type 'stack-ghci
        haskell-indent-spaces 4)
  (add-to-list 'haskell-font-lock-quasi-quote-modes '("yamlQQ" . yaml-mode))
  (add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-O0")
  (add-to-list 'haskell-process-args-stack-ghci "--ghci-options=-fshow-loaded-modules")
  (add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
  (add-to-list 'auto-mode-alist '("\\.hss\\'" . haskell-mode))
  (add-to-list 'interpreter-mode-alist '("stack" . haskell-mode))
  (add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package wgrep
  :config
  (setq wgrep-enable-key (kbd "C-c C-w")) ; change to wgrep mode
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper counsel-grep ivy-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config (company-prescient-mode +1))

;; Programming language support and utilities

(use-package lsp-mode
  :hook ((c-mode         ; clangd
          c-or-c++-mode  ; clangd
          java-mode      ; eclipse-jdtls
          js-mode        ; typescript-language-server
          python-mode    ; pyls
          dart-mode      ; dart analysis server
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))

(use-package lsp-java
  :after lsp)

(use-package company-lsp
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil))
  (use-package org-bullets :hook (org-mode . org-bullets-mode)))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package yasnippet-snippets
  :config
  (yas-global-mode +1)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  (advice-add 'company-complete-common
              :before
              #'(lambda ()
                  (setq my-company-point (point))))
  (advice-add 'company-complete-common
              :after
              #'(lambda ()
                  (when (equal my-company-point (point))
                    (yas-expand)))))

(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package csv-mode :ensure t)

(use-package wp-readme
  :load-path "~/workspace/CodyReichert/wp-readme-mode")

(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook 'add-node-modules-path)))

(use-package flowmacs
  :load-path "~/workspace/CodyReichert/flowmacs"
  :config
  ;; Ensure `flowmacs' uses the project local flow binary
  (defun cody/flowmacs-local-flow ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (flow (and root (expand-file-name
                            "node_modules/.bin/flow"
                            root))))
      (when (and flow (file-executable-p flow))
        (setq-local flowmacs/+flow+ flow))))
  (add-hook 'flowmacs-mode-hook 'cody/flowmacs-local-flow)
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook 'flowmacs-mode)))

(use-package prettier-js
  :ensure t
  :config
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook 'prettier-js-mode)))

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :mode (("\\.jsx?$" . web-mode)
         ("\\.php$" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :custom
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-auto-quote-style nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlint javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-haskell
  :ensure t
  :hook haskell-mode)

(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode))
  :config (setq emmet-expand-jsx-className? t))

(use-package format-all
  :preface
  (defun cody/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  (defun format-document()
    "Auto-format whole buffer (VSCode syntax)."
    (interactive)
    (cody/format-code)))

;; Miscellaneous

(use-package diminish
  :demand t)

(use-package which-key
  :diminish
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

(provide 'init)
;;; init.el ends here
