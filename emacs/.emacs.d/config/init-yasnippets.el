;;; init-yasnippets.el --- Yasnippets settings

;;; Commentary:
;;; yasnippets settings

;;; Code:
(add-to-list 'load-path "~/.emacs.d/es6-snippets")

(require 'yasnippet)
;; (require 'react-snippets)
(require 'es6-snippets)


(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)


(provide 'init-yasnippets)
;;; init-yasnippets.el ends here
