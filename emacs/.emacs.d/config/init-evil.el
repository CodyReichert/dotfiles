;;; init-evil.el --- Evil Mode and friends

;;; Commentary:
;;; evil-mode settings

;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'key-chord)

(global-evil-leader-mode 1)

(setq evil-leader/in-all-states t)

;;; evil leader (<SPC> - cmd)
(evil-leader/set-leader "SPC")

(evil-leader/set-key "f" 'projectile-or-helm-find-file)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "s" 'switch-window)
(evil-leader/set-key "k" 'kill-this-buffer)
(evil-leader/set-key "b" 'helm-buffers-list)

;; (setq evil-leader/no-prefix-mode-rx '("*-mode"))

(evil-mode 1)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)


(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)

(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)


(provide 'init-evil)

;;; init-evil.el ends here
