;;; init-utils.el --- Utils

;;; Commentary:
;;; utils

;;; Code:
(require 'moz)

;;; Chromium reload browser (<C-x C-r>)
(defun chrome-reload ()
  "Reload Chrome/Chromium from emacs. Requires the npm package 'chromix'"
  (interactive)
  (shell-command "chromix with \"http://.*.dev\" reloadWithoutCache"))
  ;; (shell-command "chromix with file reloadWithoutCache"))

(define-key global-map "\C-x\C-r" 'chrome-reload)

;; Chromium reload on save minor mode
(define-minor-mode chromium-reload-on-save-mode
  "Chromium Reload on Save minor mode. When activated in a buffer, chromium will
   reload on buffer save"
  nil "Chromium Reload" nil
  (if chromium-reload-on-save-mode
      (add-hook 'after-save-hook 'chrome-reload nil t)
    (remove-hook 'after-save-hook 'chrome-reload t)))


;; Firefox reload on save
(define-minor-mode moz-reload-on-save-mode
  "Moz Reload On Save Minor Mode. When activated in a buffer, firefox will
   reload on save"
  nil " Reload" nil
  (if moz-reload-on-save-mode
      (add-hook 'after-save-hook 'moz-firefox-reload nil t)
    (remove-hook 'after-save-hook 'moz-firefox-reload t)))


(defun moz-firefox-reload ()
  "Reload Firefox with MozRepl."
  (interactive)
  (comint-send-string (inferior-moz-process) "BrowserReload();"))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")


(defun latex-compile(x)
  "Compile TeX files to pdf and reload chromium to view changes"
  (interactive "File: ")
  (setq filename (concat x))
  (setq command "pdflatex ")
  (setq texCompile (concat command filename))
  (shell-command texCompile)
  (shell-command "chromix with file reloadWithoutCache")
  (message "success"))

(define-key global-map "\C-c\C-w" 'latex-compile)

(eval-after-load 'tex-mode
  '(define-key tex-mode-map [f5] 'latex-compile))


(defun npm-command (command)
  "start run a script from an npm package.json (it works, but needs some work)"
  (interactive
   (let* ((default-directory (locate-dominating-file default-directory "package.json"))
          (command (concat "npm run "))
          (compilation-buffer-name-function (lambda (mode) (concat "npm-run-" command))))
     (list (compilation-read-command command))))
  (compile command))


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
