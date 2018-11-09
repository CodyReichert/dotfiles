;;; init-evil.el --- Evil Mode and friends

;;; Commentary:
;;; evil-mode settings

;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'key-chord)
(require 'web-mode)
(require 'helm-spotify-plus)


(global-evil-leader-mode 1)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader "SPC")

(evil-leader/set-key "j" 'Control-X-prefix)

(evil-leader/set-key "f" 'projectile-or-helm-find-file)
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "k" 'kill-this-buffer)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "v" 'er/expand-region)

;; flow-mode.el provided these functions
(evil-leader/set-key "j \\" 'flowmacs/flow-pretty-status)
(evil-leader/set-key "j RET" 'flowmacs/flow-status)
(evil-leader/set-key "j d" 'flowmacs/jump-to-def)
(evil-leader/set-key "j r" 'flowmacs/find-refs)
(evil-leader/set-key "j s" 'flowmacs/suggest-types)
(evil-leader/set-key "j t" 'flowmacs/type-at-pos)

(evil-leader/set-key "g c" 'avy-goto-char)
(evil-leader/set-key "g C" 'avy-goto-char2)
(evil-leader/set-key "g l" 'avy-goto-line)
(evil-leader/set-key "g w" 'avy-goto-word-0)
(evil-leader/set-key "g W" 'avy-goto-word-1)

(evil-leader/set-key "r w" 'web-mode)

(evil-leader/set-key "p" 'ace-window)
(evil-leader/set-key "o" 'aw-flip-window)
(evil-leader/set-key "i" 'ace-swap-window)

(evil-leader/set-key "e n" 'flycheck-next-error)
(evil-leader/set-key "e p" 'flycheck-previous-error)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; web-mode keys
(evil-leader/set-key "a k" 'web-mode-attribute-kill)
(evil-leader/set-key "e w" 'web-mode-element-wrap)
(evil-leader/set-key "e r" 'web-mode-element-rename)
(evil-leader/set-key "e v" 'web-mode-element-vanish)

;; helm-spotify-plus
(evil-leader/set-key "s s" 'helm-spotify-plus)
(evil-leader/set-key "s t" 'helm-spotify-plus-toggle-play-pause)
(evil-leader/set-key "s n" 'helm-spotify-plus-next)
(evil-leader/set-key "s p" 'helm-spotify-plus-previous)


(evil-mode 1)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)

(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)

(provide 'init-evil)

;;; init-evil.el ends here
