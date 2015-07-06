;;; init-orgpage.el --- Orgpage settings

;;; Commentary:
;;; orgpage settings

;;; Code:
(require 'org-page)
(setq op/repository-directory "~/workspace/blog/blog")
(setq op/site-domain "http://codyreichert.github.io")
(setq op/personal-github-link "https://github.com/CodyReichert")
(setq op/site-main-title "The One True Blog")
(setq op/site-sub-title "Emacs, Programming, and Arch Linux")
(setq op/personal-disqus-shortname "theonetrueblog")

(setq op/theme-root-directory "~/workspace/blog/themes")
(setq op/theme 'cody)

(setq op/item-cache nil)

;; (setq op/theme 'mdo)

(provide 'init-orgpage)
;;; init-orgpage.el ends here
