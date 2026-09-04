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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package))

;;; Settings without corresponding packages

(use-package emacs
  :preface
  (defvar cody/indent-width 2)
  :config
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq show-trailing-whitespace t)
  (setq ad-redefinition-action 'accept)
  (add-hook 'after-save-hook 'delete-trailing-whitespace)
  (setq user-full-name "Cody Reichert"
        frame-title-format '("Emacs")
        ring-bell-function 'ignore
        default-directory "~/"
        frame-resize-pixelwise t
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        vc-follow-symlinks t ; Don't ask!
        load-prefer-newer t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-fill-column 90)
  (global-display-line-numbers-mode)
  ;; Store backup/autosave files outside of the project
  (setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
  (setq-default line-spacing 2
                indent-tabs-mode nil
                tab-width cody/indent-width)
  (defun disable-delete-trailing-whitespace ()
    (remove-hook 'after-save-hook 'delete-trailing-whitespace))

  (defun sudo-find-file (file-name)
    "Like find file, but opens the file as root."
    (interactive "FSudo Find File: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
      (find-file tramp-file-name)))

  ;; --- Theme Toggle Function ---
  (defun toggle-custom-themes ()
    "Toggle between 'codys-custom-dark' and 'codys-custom-light' themes."
    (interactive)

    ;; Ensure the theme directory is in the path
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

    ;; Check which theme is currently active and switch to the other
    (if (memq 'codys-custom-dark custom-enabled-themes)
        ;; If dark is active, disable it and load light
        (progn
          (disable-theme 'codys-custom-dark)
          (load-theme 'codys-custom-light t)
          (message "Switched to Cody's Custom Light Theme"))
      ;; Otherwise switch to dark theme
      (progn
        (when (memq 'codys-custom-light custom-enabled-themes)
          (disable-theme 'codys-custom-light))
        (load-theme 'codys-custom-dark t)
        (message "Switched to Cody's Custom Dark Theme"))))

  ;; Toggle light and dark themes with [F5]
  (global-set-key (kbd "<f5>") #'toggle-custom-themes))

;;; Built-in packages

(use-package startup
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

(use-package window
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
        make-backup-files nil
        create-lockfiles nil))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config (setq eldoc-idle-delay 0.5))

(use-package xref
  :ensure nil
  :config
  (define-key prog-mode-map (kbd "s-b") #'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") #'xref-go-back))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset cody/indent-width)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

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

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function #'split-window-horizontally))

(use-package flyspell
  :ensure nil
  :config (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

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

(use-package conf-mode
  :ensure nil
  :mode (("\\.flowconfig$" . conf-mode)
         ("\\.npmrc$" . conf-mode)
         ("\\.prettierrc$" . conf-mode))
  :config (setq js-indent-level cody/indent-width))

(use-package epg
  :config (setq epg-pinentry-mode 'loopback))

;;; Third-party Packages

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init (setq scroll-conservatively 101
              scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; GUI enhancements
(use-package mood-line
  ;; Enable mood-line
  :config
  (setq mood-line-format mood-line-format-default-extended)
  (mood-line-mode))

(use-package dashboard-hackernews
  :config
  (require 'json))

(use-package dashboard
  :after dashboard-hackernews
  :config
  (setq dashboard-banner-logo-title "Dangerously powerful" ; set the title
        dashboard-startup-banner 'logo ; show the logo in the banner area
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 5) ; this is where the magic happens
                          (bookmarks . 5)
                          (hackernews . 5)
                          (agenda . 5))
        dashboard-center-content t ; center the dashboard
        dashboard-week-agenda t) ; set the agenda
  (dashboard-setup-startup-hook))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package codys-custom-dark-theme
  :ensure nil
  :init (add-to-list
         'custom-theme-load-path "~/.emacs.d/themes/")
  :load-path "~/.emacs.d/themes/"
  :demand
  :config
  (load-theme 'codys-custom-dark t))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.25))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package centaur-tabs
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
        ;; centaur-tabs-label-fixed-length 15
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 100)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-group-by-projectile-project)))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character 9615) ; left-align vertical bar
  (setq highlight-indent-guides-auto-character-face-perc 20))

(use-package rainbow-mode
  :hook ((web-mode . rainbow-mode)
         (emacs-lisp-mode . rainbow-mode)))

;; (use-package emojify
;;   :config (global-emojify-mode))

;; (use-package emojify-logos)

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Vi keybindings

(use-package evil
  :init
  (setq evil-shift-width cody/indent-width)
  (setq-default evil-kill-on-visual-paste nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
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
  (define-key evil-normal-state-map (kbd "x") 'gptel-menu)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'cody/save-and-kill-this-buffer))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode +1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-mode 1))

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
  (diff-hl-insert ((t (:foreground "#55bb55" :background "unspecified"))))
  (diff-hl-delete ((t (:foreground "#ff6666" :background "unspecified"))))
  (diff-hl-change ((t (:foreground "#99bbdd" :background "unspecified"))))
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;; Searching/sorting enhancements & project management

(use-package flx)

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

(use-package ivy
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
  :config
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode +1))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list
        '(counsel-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))))  :config
           (ivy-rich-mode)
           (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c f") #'projectile-find-file) ; counsel
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep) ; counsel
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'alien
        projectile-completion-system 'ivy))

(use-package ace-window
  :ensure t)

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (setq evil-leader/in-all-states t)
  (evil-leader/set-leader "SPC")

  ; global
  (evil-leader/set-key "w" 'save-buffer)
  (evil-leader/set-key "j" 'Control-X-prefix)
  (evil-leader/set-key "b" 'counsel-switch-buffer)
  (evil-leader/set-key "f" 'counsel-projectile-find-file)
  (evil-leader/set-key "g" 'counsel-git-grep)
  (evil-leader/set-key "o" 'aw-flip-window)
  (evil-leader/set-key "i" 'window-swap-states)
  (evil-leader/set-key "x" 'gptel-menu)
  (evil-leader/set-key "k"
    '(lambda ()
       (interactive)
       (kill-this-buffer)
       (previous-buffer)))

  ; flycheck-*
  (evil-leader/set-key "e n" 'flycheck-next-error)
  (evil-leader/set-key "e p" 'flycheck-previous-error)

  ; flowmacs/*
  (evil-leader/set-key "j r" 'flowmacs-mode)
  (evil-leader/set-key "j f" 'flowmacs/find-refs)
  (evil-leader/set-key "j t" 'flowmacs/type-at-pos)
  (evil-leader/set-key "j d" 'flowmacs/jump-to-def)
  (evil-leader/set-key "j s" 'flowmacs/suggest-types)
  (evil-leader/set-key "j RET" 'flowmacs/status)

  ; web-mode-*
  (evil-leader/set-key "r r" 'web-mode)
  (evil-leader/set-key "r a k" 'web-mode-attribute-kill)
  (evil-leader/set-key "r a t" 'web-mode-attribute-transpose)
  (evil-leader/set-key "r a s" 'web-mode-attribute-select)
  (evil-leader/set-key "r a k" 'web-mode-attribute-kill)
  (evil-leader/set-key "r a t" 'web-mode-attribute-transpose)
  (evil-leader/set-key "r a s" 'web-mode-attribute-select)
  (evil-leader/set-key "r t a" 'web-mode-tag-attributes-sort)
  (evil-leader/set-key "r t m" 'web-mode-tag-match)
  (evil-leader/set-key "r t s" 'web-mode-tag-select)
  (evil-leader/set-key "r e w" 'web-mode-element-wrap)
  (evil-leader/set-key "r e c" 'web-mode-element-clone)
  (evil-leader/set-key "r e r" 'web-mode-element-rename)
  (evil-leader/set-key "r e v" 'web-mode-element-vanish)
  (evil-leader/set-key "r e u" 'web-mode-element-parent)

  ;; evil commands
  ;; (evil-leader/set-key "n" 'evil-search-word-backward)
  ;; (evil-leader/set-key "m" 'evil-search-word-forward)
  ;;
  ;; web-mode (legacy)
  ;; (evil-leader/set-key "a k" 'web-mode-attribute-kill)
  ;; (evil-leader/set-key "e w" 'web-mode-element-wrap)
  ;; (evil-leader/set-key "e r" 'web-mode-element-rename)
  ;; (evil-leader/set-key "e v" 'web-mode-element-vanish)
  )

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
        '(:not swiper counsel-grep counsel-switch-buffer))
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
          ;; js-mode        ; typescript-language-server
          python-mode    ; pyls
          dart-mode      ; dart analysis server
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))

(use-package lsp-java
  :after lsp)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

;; (use-package company-emojify)

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil))
  (use-package org-bullets :hook (org-mode . org-bullets-mode)))

(use-package yasnippet)

(use-package es6-snippets
  :load-path "~/workspace/CodyReichert/es6-snippets")

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

(use-package yaml-mode :ensure t)
(use-package csv-mode :ensure t)

(use-package dotenv-mode
  :mode (("\\.env\\'" . dotenv-mode)
         ("\\.env.sample\\'" . dotenv-mode)))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package wp-readme
  :load-path "~/workspace/CodyReichert/wp-readme-mode")

(use-package web-mode
  :ensure t
  :mode (("\\.jsx$" . web-mode)
         ("\\.js$" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.php$" . web-mode)
         ("\\.html$" . web-mode))
  :custom
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-auto-quote-style nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  :config
  (defun cody/set-web-mode-indent-width ()
    (message web-mode-content-type)
    (if (string-equal-ignore-case web-mode-content-type "json")
        (setq web-mode-code-indent-offset 2)
      (setq web-mode-code-indent-offset 4)))
  (add-hook 'web-mode-hook 'cody/set-web-mode-indent-width))

(use-package add-node-modules-path
  :ensure t
  :hook ((web-mode . add-node-modules-path)
         (markdown-mode . add-node-modules-path)
         (json-mode . add-node-modules-path)))

(use-package flowmacs
  :load-path "~/workspace/CodyReichert/flowmacs"
  :config
  (defun cody/flowmacs-local-flow (&optional start)
    (let* ((root (locate-dominating-file
                  (or start (buffer-file-name) default-directory)
                  "node_modules"))
           (flow (and root (expand-file-name
                            "node_modules/.bin/flow"
                            root))))
      (if (and flow (file-executable-p flow))
          (setq-local flowmacs/+flow+ flow)
        (when root  ;; Check if root is non-nil before recursion
          (cody/flowmacs-local-flow (file-name-directory (directory-file-name root)))))))
  (add-hook 'flowmacs-mode-hook 'cody/flowmacs-local-flow)
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook 'flowmacs-mode)))

(use-package prettier-js
  :ensure t
  :hook ((web-mode . prettier-js-mode)
         (markdown-mode . prettier-js-mode)
         (json-mode . prettier-js-mode))
  :config (add-node-modules-path))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode))
  :config (setq emmet-expand-jsx-className? t))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlint javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-haskell
  :ensure t
  :hook haskell-mode)

(use-package arduino-cli-mode)

(use-package tramp
  :ensure t
  :config
  (setq tramp-verbose 10))

;; Pass integration
(use-package password-store)

;; Gptel

(use-package gptel
  :config
  ; Ollama model
  (gptel-make-ollama "Ollama"      ;Any name of your choosing
    :host "localhost:11434"        ;Where it's running
    :stream t                      ;Stream responses
    :models '(llama3.1))           ;List of models

  (gptel-make-ollama "Gemma Verbose 7B" ;Any name of your choosing
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '(gemma-verbose:7b))        ;List of models

  (gptel-make-ollama "gemma-dolly-run2"
    :host "localhost:11434"             ;Where it's running
    :stream t                           ;Stream responses
    :models '(gemma-dolly-run2))        ;List of models

  (setq
   gptel-model 'gemma-dolly-run2
   gptel-backend (gptel-make-ollama "gemma-dolly-run2"
                   :host "localhost:11434"
                   :stream t
                   :models '(gemma-dolly-run2)))

  )

(use-package hass
  :ensure t
  :config
  (setq hass-host "home.codys.club")
  (setq hass-apikey (getenv "HASSIO_API_KEY"))
  (setq hass-port 443)
  (setq hassdash-layouts
      '((default . ; Key for dashboard. Shows up with completing-read when calling `hass-dash-open'.
                 ((hass-dash-group                 ; Create a widget group.
                   :title "Home Assistant - Emacs" ; Give the group a title at the top.
                   :format "%t\n\n%v"              ; %t is where the title goes and %v is the widget it owns.
                   (hass-dash-group                ; Create a subgroup of widgets.
                    :title "My office"
                    :title-face outline-2   ; Give it a unique face to make it stand out.
                    (hass-dash-toggle :entity-id "light.office_lamp")
                    (hass-dash-toggle :entity-id "light.desk_lamp")
                    (hass-dash-toggle :entity-id "light.office_fan_light")
                    (hass-dash-toggle :entity-id "light.office_fan_light_2")
                    (hass-dash-group
                     :title "Media"
                     :title-face outline
                     (hass-dash-toggle :entity-id "media_player.spotify_cody"))
                    (hass-dash-group
                     :title "Monitors"
                     :title-face outline
                     (hass-dash-toggle :entity-id "media_player.odyssey")
                     (hass-dash-toggle :entity-id "media_player.m5"))))
                  (hass-dash-group :title "Bedroom" :format "\n\n%t\n\n%v"
                                   (hass-dash-toggle :entity-id "light.bedside_table_lamp")
                                   (hass-dash-toggle :entity-id "light.dresser_lamp"))))

        (simple . ; Declaring a top-level group is optional and implied.
                ((hass-dash-toggle :entity-id "scene.office_all_on")
                 (hass-dash-toggle :entity-id "scene.office_all_off"))))))

;; Miscellaneous
(use-package which-key
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (setq exec-path-from-shell-arguments '("-c"))
            (exec-path-from-shell-initialize)))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

; The default does not like the ~ before > so do a kludge
(setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")
(setq initial-scratch-message (format ";; startup took %s\n\n" (emacs-init-time)))

(provide 'init)
;;; init.el ends here
