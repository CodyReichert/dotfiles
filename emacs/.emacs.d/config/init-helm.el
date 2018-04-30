;;; init-helm.el --- Helm settings

;;; Commentary:
;;; helm settings

;;; Code:
(require 'helm)
(require 'helm-config)
(require 'helm-mpd)


(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "M-y"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))


(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     nil
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)


(setq helm-echo-input-in-header-line t)


(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)


(defun projectile-or-helm-find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'helm-projectile-find-file
     'helm-find-files)))


;; Enable 'skip-boring-files' to hide things like temporary files from
;; 'find-files' and similar commands.
(setq helm-boring-file-regexp-list
  '("\\.#" "\\#.*#$" "\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
    "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$"))
(setq helm-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))
(setq helm-ff-skip-boring-files t)


(helm-mode 1)
(helm-projectile-on)


(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'projectile-or-helm-find-file)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'init-helm)
;;; init-helm.el ends here
