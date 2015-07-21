;;; init-evil.el --- Evil Mode and friends

;;; Commentary:
;;; evil-mode settings

;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'key-chord)


(global-evil-leader-mode 1)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader "SPC")

(evil-leader/set-key "f" 'projectile-or-helm-find-file)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "k" 'kill-this-buffer)
(evil-leader/set-key "b" 'helm-buffers-list)

(evil-leader/set-key "g c" 'avy-goto-char)
(evil-leader/set-key "g C" 'avy-goto-char2)
(evil-leader/set-key "g l" 'avy-goto-line)
(evil-leader/set-key "g w" 'avy-goto-word-0)
(evil-leader/set-key "g W" 'avy-goto-word-1)

(evil-leader/set-key "p" 'ace-window)
(evil-leader/set-key "o" 'aw-flip-window)
(evil-leader/set-key "i" 'ace-swap-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

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
