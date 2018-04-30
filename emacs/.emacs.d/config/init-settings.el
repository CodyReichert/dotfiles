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

(set-fringe-mode nil)

(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t)

(global-set-key (kbd "C-x o") 'ace-window)

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

(defalias 'yes-or-no-p 'y-or-n-p)


;; Disable alarm bell. Flash the mode-line instead
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil ring-bell-function #'my-terminal-visible-bell)

;;;;;;;;;;;;;;;;;;
;; Theme/Font   ;;
;;;;;;;;;;;;;;;;;;
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


;; Show whitespace / delete it on save
(setq show-trailing-whitespace t)
(add-hook 'after-save-hook 'delete-trailing-whitespace)


(provide 'init-settings)
;;; init-settings.el ends here
