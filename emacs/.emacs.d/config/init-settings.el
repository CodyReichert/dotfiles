;;; init-settings.el --- Mostly aesthetic settings

;;; Commentary:
;;; Settings and Initializations

;;; Code:
(add-to-list 'load-path "~/.emacs.d/wp-readme-mode")

(require 'highlight-parentheses)
(require 'switch-window)
(require 'smooth-scroll)
(require 'wp-readme)

(setq initial-buffer-choice '(lambda () (eshell)))

;;; slime/lisp/etc
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(slime-setup '(slime-fancy slime-mrepl slime-banner slime-tramp
	       slime-xref-browser slime-highlight-edits
	       slime-sprof))

(set-fringe-mode nil)

(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t)

(global-set-key (kbd "C-x o") 'switch-window)

(global-set-key (kbd "C-c C-v") 'er/expand-region)

(menu-bar-mode -1)

(setq scroll-margin 3
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Theme/Font                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq font-lock-maximum-decoration t)

(setq font-use-system-font nil)

(tool-bar-mode -1)

(menu-bar-mode -1)

(column-number-mode -1)

(set-scroll-bar-mode nil)

(load-theme 'afternoon t)

(set-frame-font "Roboto Mono 10")

(face-spec-reset-face 'mode-line)

(face-spec-reset-face 'mode-line-inactive)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#969896")

(setq face-font-family-alternatives '
    (("Roboto Mono")
     ("Monospace" "courier" "fixed")
     ("courier" "CMU Typewriter Text" "fixed")
     ("Sans Serif" "helv" "helvetica" "arial" "fixed")
     ("helv" "helvetica" "arial" "fixed")))


(provide 'init-settings)
;;; init-settings.el ends here
