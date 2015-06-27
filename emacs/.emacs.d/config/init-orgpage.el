;;; init-orgpage.el --- Orgpage settings

;;; Commentary:
;;; orgpage settings

;;; Code:
(require 'org-page)
(setq op/repository-directory "/home/cody/workspace/org/blog")
(setq op/site-domain "http://codyreichert.github.io")
(setq op/personal-github-link "https://github.com/CodyReichert")
(setq op/site-main-title "The One True Blog")
(setq op/site-sub-title "Emacs, programming, and whatever else comes to mind")
(setq op/personal-disqus-shortname "theonetrueblog")
(setq op/theme 'mdo)

(provide 'init-orgpage)
;;; init-orgpage.el ends here
