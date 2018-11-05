;;; init-packages.el --- initialize packages and repositories

;;; Commentary:
;;;
;;; On startup, initialize packages and repositories.  Check for all
;;; of the packages listed and if they don't exist, install them.

;;; Code:

(setq package-list '(tabulated-list add-node-modules-path
                                    afternoon-theme
                                    ac-slime
                                    ace-window
                                    auctex
                                    auto-complete
                                    coffee-mode
                                    cyberpunk-theme
                                    dash
                                    emmet-mode
                                    emojify
                                    epl
                                    evil
                                    evil-leader
                                    evil-numbers
                                    evil-org
                                    expand-region
                                    flycheck
                                    ghci-completion
                                    goto-chg
                                    haskell-mode
                                    helm
                                    helm-git-grep
                                    helm-projectile
                                    highlight-parentheses
                                    ido-vertical-mode
                                    json-mode
                                    json-reformat
                                    key-chord
                                    latex-extra
                                    magit
                                    markdown-mode
                                    org
                                    org-page
                                    php-mode
                                    phpunit
                                    pkg-info
                                    pkgbuild-mode
                                    popup
                                    powerline
                                    projectile
                                    rust-mode
                                    s
                                    shakespeare-mode
                                    slime
                                    smart-mode-line
                                    smart-mode-line-powerline-theme
                                    smooth-scroll
                                    switch-window
                                    undo-tree
                                    web-mode
                                    yaml-mode
                                    yasnippet
                                    ))

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(provide 'init-packages)

;;; init-packages.el ends here
