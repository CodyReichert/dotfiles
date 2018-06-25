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
