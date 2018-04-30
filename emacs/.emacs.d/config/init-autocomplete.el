;;; init-autocomplete.el --- Autocomplete settings

;;; Commentary:
;;; autocomplete settings

;;; Code:
(require 'auto-complete)
(require 'auto-complete-config)


(global-auto-complete-mode t)
(setq ac-auto-show-menu t)

(set-default 'ac-sources
             '(
               ac-source-filename
               ;; ac-source-files-in-current-dir
               ac-source-imenu
               ac-source-words-in-buffer
               ac-source-words-in-all-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-yasnippet
               ac-source-dictionary
               ;; ac-source-gtags
               ;; ac-source-ispell
               ))

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
