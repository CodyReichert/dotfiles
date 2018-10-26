;;; init-utils.el --- Utils

;;; Commentary:
;;; utils

;;; Code:
(defun xah-escape-quotes ()
    "Replace 「\"」 by 「\\\"」 in current line or text selection.
     That is, add 1 backslash in front of double quote (Unicode codepoint 34).
     See also: `xah-unescape-quotes'
     Version 2015-01-24
     URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
    "
    (interactive)
    (let (p1 p2)
      (if (use-region-p)
          (progn (setq p1 (region-beginning))
                 (setq p2 (region-end)))
        (progn (setq p1 (line-beginning-position))
               (setq p2 (line-end-position))))
      (save-excursion
        (save-restriction
          (narrow-to-region p1 p2)
          (goto-char (point-min))
          (while (search-forward "\"" nil t)
                      (replace-match "\\\"" 'FIXEDCASE 'LITERAL))))))

(define-key global-map "\C-c\C-y" 'xah-escape-quotes)


;; open file as sudo with tramp
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))



(provide 'init-utils)
;;; init-utils.el ends here
