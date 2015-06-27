;;; init-compilation.el --- Compilation settings

;;; Commentary:
;;; compilation settings

;;; Code:
(require 'compile)
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-scroll-output t)

(autoload 'compilation-always-kill-mode "compilation-always-kill" nil t)
(compilation-always-kill-mode 1)


(provide 'init-compilation)
;;; init-compilation.el ends here
