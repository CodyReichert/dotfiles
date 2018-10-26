;;; init-sml.el --- Smart Mode Line Settings

;;; Commentary:
;;; smart-mode-line settings

;;; Code:
(require 'smart-mode-line)

(defun my/sml-setup-hook ()
  "Enable sml after Emacs has loaded."
  (sml/setup))

(add-hook 'after-init-hook 'my/sml-setup-hook)

(provide 'init-sml)
;;; init-sml.el ends here
