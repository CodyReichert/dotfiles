;;; init-slime.el --- Settings for SLIME/Common Lisp

;;; Commentary:
;;; Settings for Slime, Swank, and Common Lisp

;;; Code:
(add-to-list 'load-path "~/.emacs.d/slime-repl-ansi-color")

(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

(slime-setup '(slime-fancy slime-mrepl slime-banner slime-tramp
	       slime-xref-browser slime-highlight-edits
	       slime-sprof slime-repl-ansi-color))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(define-key slime-repl-mode-map (kbd "C-p") 'slime-repl-backward-input)
(define-key slime-repl-mode-map (kbd "C-n") 'slime-repl-forward-input)

(define-key slime-repl-mode-map (kbd "C-c s") 'slime-repl-clear-buffer)

(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))


(provide 'init-slime)
;;; init-slime.el ends here.
