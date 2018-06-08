;;; init-org.el --- Org-mode and friends

;;; Commentary:
;;; org-mode settings

;;; Code:
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "~/dotfiles/emacs/.emacs.d/scripts")

(require 'evil-org)
(require 'org-list)
(require 'org)
(require 'org-habit)
(require 'org-contacts)

(add-hook 'org-mode-hook '(lambda () (auto-fill-mode 1)))

(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

(setq org-directory "~/workspace/organizer")
(setq org-default-notes-file "~/workspace/organizer/notes.org")

(setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "DONE(d@/!)")))


(setq org-tag-alist '(("@work"     . ?b)
                      ("@home"     . ?h)
                      ("@writing"  . ?w)
                      ("@errands"  . ?e)
                      ("@coding"   . ?c)
                      ("@reading"  . ?r)
                      ("@people"   . ?p)
                      ("@computer" . ?l)))


(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "Red2" :background nil :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :background nil :weight bold))
        ("SCHEDULED" . (:foreground "Blue" :background nil :weight bold))
        ("WAITING" . (:foreground "cyan" :background nil :weight bold))
        ("DONE" . (:foreground "green" :background nil :weight bold))))


(setq org-tag-faces
      '(("@work"
         :foreground "Green3"
         :background nil
         :weight bold)
        ("@home"
         :foreground "LightGray"
         :background nil
         :weight bold)
        ("@writing"
         :foreground "DarkGoldenrod"
         :background nil
         :weight bold)
        ("@errands"
         :foreground "LightSeaGreen"
         :background nil
         :weight bold)
        ("@people"
         :foreground "Orange"
         :background nil
         :weight bold)
        ("@coding"
         :foreground "LightSteelBlue"
         :background nil
         :weight bold)
        ("@reading"
         :foreground "Salmon"
         :background nil
         :weight bold)
        ("@computer"
         :foreground "Orchid"
         :background nil
         :weight bold)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t :foreground "#Dcdcdc" :border nil :bold nil :overline nil :height 1.2 :background nil)))
 '(org-level-2 ((t :foreground "#Eedd82" :border nil :bold nil :overline nil :height 1.1 :background nil)))
 '(org-level-3 ((t :foreground "#Ffa500" :height 1.0 :background nil)))
 '(org-level-4 ((t :foreground "#F5deb3" :background nil))))


(add-to-list 'org-checkbox-statistics-hook
             (function ndk/checkbox-list-complete))


(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)


(defvar org-tasks-file   "~/workspace/organizer/tasks.org")
(defvar org-buiness-file "~/workspace/organizer/business.org")
(defvar org-notes-file   "~/workspace/organizer/notes.org")
(defvar org-journal-file "~/workspace/organizer/journal.org")
(defvar org-people-file  "~/workspace/organizer/people.org")
(defvar org-contacts-file  "~/workspace/organizer/contacts.org")
(defvar org-org-file "~/workspace/organizer/organizer.org") ;;deprecated


(defvar my/org-basic-task-template "* TODO %^{Task}
   SCHEDULED: %^t
   %<%Y-%m-%d %H:%M>
   :PROPERTIES:
   :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
   :END:
   %?
" "Basic task data")


(defvar my/org-email-capture-template "* TODO (%:from) %^{Task}
   SCHEDULED: %^t
   %<%Y-%m-%d %H:%M>
   :PROPERTIES:
   :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
   :END:
   Subject: %:subject
   Link: %a
   %?
" "Email Capture Template")



(defvar my/org-habit-template "* TODO %^{Task}
   SCHEDULED: %^t
   %<%Y-%m-%d %H:%M>
   :LOGBOOK:
   :END:
   :PROPERTIES:
   :STYLE: habit
   :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
   :END:
   %?
" "Basic task data")


(defvar my/org-idea-template "* TODO %^{Task}
   :PROPERTIES:
   :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
   :END:
   %?
" "Idea template")


(defvar my/org-snippet-template "* %^{Title} \n
   %^G
   %^{prompt}
   #+BEGIN_SRC %^{prompt}
   %i
   #+END_SRC
   %?
" "Code snippet template")


(defvar crr/org-contact-template "* %(org-contacts-template-name)
   :PROPERTIES:
   :EMAIL: %(org-contacts-template-email)
   :PHONE:
   :NOTE:
   :END:
" "Contact template")


(setq org-capture-templates `(
        ;;
        ;; tasks
        ;;
        ("t" "Tasks" entry
         (file+headline org-tasks-file "Tasks")
         ,my/org-basic-task-template)
        ("T" "Quick task" entry
         (file+headline org-tasks-file "Tasks")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("b" "Business task" entry
         (file+headline org-business-file "Tasks")
         ,my/org-basic-task-template)
        ("p" "People task" entry
         (file+headline org-people-file "Tasks")
         ,my/org-basic-task-template)
        ("h" "Habit" entry
         (file+headline org-tasks-file "Habits")
         ,my/org-habit-template)
        ("m" "Mail TODO" entry
         (file+headline org-tasks-file "Tasks")
         ,my/org-email-capture-template)
        ("c" "New Contact" entry
         (file org-contacts-file)
         ,crr/org-contact-template)


        ;;
        ;; ideas
        ;;
        ("e" "Emacs idea" entry
         (file+headline org-notes-file "Emacs")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("i" "Idea" entry
         (file+headline org-notes-file "Ideas")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("w" "Blog prompt" entry
         (file+headline org-notes-file "Blog Prompts/Ideas")
         "* TODO %^{Task}")

        ;;
        ;; notes / journal
        ;;
        ("j" "Journal entry" plain
         (file+datetree org-journal-file)
         "%K - %?\n"
         :unnarrowed t)
        ("J" "Journal entry with date" plain
         (file+datetree+prompt org-journal-file)
         "%K - %?\n"
         :unnarrowed t)
        ("s" "Code Snippet" entry
         (file+headline org-notes-file "Snippets")
         ,my/org-snippet-template
         :immediate-finish t)
        ("q" "Quick note" item
         (file+headline org-notes-file "Quick notes"))))


(define-key global-map (kbd "C-c C-o") 'org-capture)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '(
                      "~/workspace/organizer/people.org"
                      "~/workspace/organizer/business.org"
                      "~/workspace/organizer/notes.org"
                      "~/workspace/organizer/tasks.org"))))


(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))

(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

(defvar my/org-agenda-contexts
  '(
    (tags-todo "+@work")
    (tags-todo "+@coding")
    (tags-todo "+@writing")
    (tags-todo "+@computer")
    (tags-todo "+@people")
    (tags-todo "+@home")
    (tags-todo "+@errands")
    )
  "Usual list of contexts.")

(defun my/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(setq org-agenda-custom-commands
      `(
        ;; unscheduled TODO's
        ("T" tags-todo "TODO=\"TODO\"-goal-routine-SCHEDULED={.+}")

        ("n" tags-todo "Notes"
         ((org-agenda-files '("~/workspace/organizer/notes.org"))))

        ("o" todo ""
         ((org-agenda-files '("~/workspace/organizer/organizer.org"))))

        ("c" todo ""
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))

        ;; Weekly review
        ("w" "Weekly review" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)))
        ("W" "Weekly review sans routines" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)
          (org-agenda-tag-filter-preset '("-routine"))))
        ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))

        ;; tag views
        ("gb" "Work" tags-todo "@work")
        ("gc" "Coding" tags-todo "@coding")
        ("gw" "Writing" tags-todo "@writing")
        ("gp" "Publishing" tags-todo "@people")
        ("gh" "Home" tags-todo "@home")
        ("ge" "Errands" tags-todo "@errands")
        ;;;


        ("0" "Top 3 by context"
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (my/org-agenda-limit-items 3)))

        (")" "All by context"
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (my/org-agenda-limit-items nil)))

        ("9" "Unscheduled top 3 by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (my/org-agenda-limit-items 3)))

        ("(" "All unscheduled by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          ))

        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))

        ("." "Waiting for" todo "WAITING")


        ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project"
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))

        ("P" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))


        ;; ("U" "Unscheduled tasks outside projects" tags-todo "-project"
        ;;  ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
        ;;   (org-tags-exclude-from-inheritance nil)
        ;;   (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
        ;;   (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
        ;; ("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\""
        ;;  ((org-tags-exclude-from-inheritance '("project"))
        ;;   (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ;; ("p." tags "+project-TODO=\"DONE\""
        ;;  ((org-tags-exclude-from-inheritance '("project"))
        ;;   (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("S" tags-todo "TODO=\"STARTED\"")
        ("2" "List projects with tasks" my/org-agenda-projects-and-tasks
         "+PROJECT"
           ((my/org-agenda-limit-items 3)))))

(define-key global-map [f7] 'org-agenda)

(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (js . t)
   (perl . t)
   (haskell . t)
))


(setq org-log-done t)


(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "Javascript" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
        (org-edit-src-code)))


(add-hook 'org-mode-hook '(lambda ()
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ))


(setq org-src-fontify-natively t)


(provide 'init-org)
;;; init-org.el ends here
