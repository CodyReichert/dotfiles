;;; init.el --- Cody Reichert's Emacs Configuration

;;; Commentary:
;;; This is yet another config file.

;;; Code:



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/scripts")

(setenv "LANG" "en_US.UTF-8")
(setenv "PATH" (concat "/home/cody/Downloads/node-v4.2.2-linux-x64/bin:" (getenv "PATH")))

(require 'init-packages)
(require 'init-utils)
(require 'init-settings)
(require 'init-sml)

(require 'init-web)
(require 'init-autocomplete)
(require 'init-compilation)
(require 'init-eshell)
(require 'init-evil)
(require 'init-haskell)
(require 'init-helm)
(require 'init-magit)
(require 'init-projectile)
(require 'init-yasnippets)

;; Current unused
;; (require 'init-ido)
;; (require 'init-circe)
;; (require 'init-slime)
;; (require 'init-mu)
;; (require 'init-org)
;; (require 'init-orgpage)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (helm-git-grep yasnippet yaml-mode web-mode switch-window smooth-scroll smart-mode-line-powerline-theme shakespeare-mode rust-mode prettier-js pkgbuild-mode phpunit php-mode org-page markdown-mode magit-gh-pulls latex-extra key-chord json-mode ido-vertical-mode highlight-parentheses helm-projectile ghci-completion flycheck-haskell expand-region evil-org evil-numbers evil-leader emojify emmet-mode cyberpunk-theme coffee-mode afternoon-theme add-node-modules-path ace-window ac-slime 0blayout))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
