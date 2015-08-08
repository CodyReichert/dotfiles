;;; init-slime.el --- Settings for SLIME/Common Lisp

;;; Commentary:
;;; Settings for Slime, Swank, and Common Lisp

;;; Code:
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(slime-setup '(slime-fancy slime-mrepl slime-banner slime-tramp
	       slime-xref-browser slime-highlight-edits
	       slime-sprof))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(define-key slime-repl-mode-map (kbd "C-p") 'slime-repl-backward-input)
(define-key slime-repl-mode-map (kbd "C-n") 'slime-repl-forward-input)

(provide 'init-slime)
;;; init-slime.el ends here.
