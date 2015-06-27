;;; init-settings.el --- Mostly aesthetic settings

;;; Commentary:
;;; Settings and Initializations

;;; Code:
(require 'highlight-parentheses)
(require 'switch-window)
(require 'smooth-scroll)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Theme/Font                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq font-lock-maximum-decoration t)

(setq font-use-system-font nil)

(tool-bar-mode -1)

(menu-bar-mode -1)

(column-number-mode -1)

(set-scroll-bar-mode nil)

(load-theme 'material t)

(setq face-font-family-alternatives '
    (("Roboto Mono")
     ("Monospace" "courier" "fixed")
     ("courier" "CMU Typewriter Text" "fixed")
     ("Sans Serif" "helv" "helvetica" "arial" "fixed")
     ("helv" "helvetica" "arial" "fixed")))


(set-face-attribute 'default nil
                     :inherit nil
                     :stipple nil
                     :background "#263238"
                     :foreground "#ffffff"
                     :inverse-video nil
                     :box nil
                     :strike-through nil
                     :overline nil
                     :underline nil
                     :slant 'normal
                     :weight 'normal
                     :height 100
                     :width 'normal
                     :foundry "unknown")

(set-frame-font "Roboto Mono 10")

(provide 'init-settings)
;;; init-settings.el ends here
