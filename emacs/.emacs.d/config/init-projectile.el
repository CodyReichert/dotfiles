;;; init-projectile.el --- Projectile settings

;;; Commentary:
;;; projectile settings

;;; Code:
(projectile-global-mode)

(global-set-key (kbd "C-c T")       'projectile-regenerate-tags)
(global-set-key (kbd "C-c t")       'projectile-test-project)
(global-set-key (kbd "C-c g")       'projectile-grep)
(global-set-key (kbd "C-c i")       'projectile-ibuffer)
(global-set-key (kbd "C-x C-d")     'projectile-switch-project)
(global-set-key [f8]                'projectile-compile-project)


(defun projectile-or-ido-find-file ()
  "A replacement for find file that will use `projectile-find-file' if
you're in a project, and `find-file' otherwise."
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'projectile-find-file
     'ido-find-file)))


(provide 'init-projectile)
;;; init-projectile.el ends here
