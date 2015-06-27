;;; init-magit.el --- Magit settings

;;; Commentary:
;;; Magit settings

;;; Code:
(setq magit-last-seen-setup-instructions "1.4.0")

(define-key global-map [f6] 'magit-status)
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-diff-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-define-key 'normal magit-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-log-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
(evil-define-key 'normal magit-diff-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)

;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-attribute  'magit-diff-foreground "white")
     (set-face-attribute  'magit-diff-background "black")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black")
       (set-face-foreground 'magit-item-highlight "white"))))


(provide 'init-magit)
;;; init-magit.el ends here
