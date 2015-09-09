;; -*-lisp-*-
;; StumpWM - init.lisp
;; Cody Reichert <codyreichert@gmail.com>

(in-package :stumpwm)

(add-to-load-path "~/.stumpwm.d/contrib/")

(add-to-load-path "/home/cody/.emacs.d/slime/swank")
(load "/home/cody/.emacs.d/slime/swank-loader.lisp")

(add-to-load-path "/home/cody/.stumpwm.d/contrib/modeline/cpu")
(add-to-load-path "/home/cody/.stumpwm.d/contrib/modeline/mem")
(add-to-load-path "/home/cody/.stumpwm.d/contrib/minor-mode/mpd")

(load-module "cpu")
(load-module "mem")
(load-module "mpd")

;; run a couple commands on startup
(stumpwm:run-shell-command "myxrandr")
(stumpwm:run-shell-command "mpd")

;; general setup
(setf *mouse-focus-policy* :sloppy) ; focus window on mouse hover
(setf stumpwm:*ignore-wm-inc-hints* t) ; fixes space around some windows
(setf *mode-line-background-color* "#333")
(setf *mode-line-foreground-color* "#ddd")

(stumpwm:set-focus-color "green")
(stumpwm:set-unfocus-color "yellow")


;; keys
(set-prefix-key (kbd "C-t"))

(define-key *root-map* (kbd "o")   "next")
(define-key *root-map* (kbd "C-o") "prev")

(define-key *root-map* (kbd "C-s") "swank")
(define-key *root-map* (kbd "c") "exec terminator")
(define-key *root-map* (kbd "C-c") "exec chromium")
(define-key *root-map* (kbd "m") "toggle-current-mode-line")
(define-key *root-map* (kbd "C-e") "exec emacs")

(define-key *root-map* (kbd "C-m") 'mpd:*mpd-map*)
(define-key mpd:*mpd-map* (kbd "C-g") "smirk-shuffle-genre")
(define-key mpd:*mpd-map* (kbd "C-a") "smirk-shuffle-artist")
(define-key mpd:*mpd-map* (kbd "C-t") "smirk-random-tracks")
(define-key mpd:*mpd-map* (kbd "a") "smirk-random-album")


(defcommand smirk-shuffle-artist (artist) ((:string "Arist: "))
  "Shuffle MPD with the given artist."
  (stumpwm:run-shell-command (concat "smirk artist " artist)))

(defcommand smirk-shuffle-genre (genre) ((:string "Genre: "))
    "Shuffle MPD with the given genre."
  (stumpwm:run-shell-command (concat "smirk genre " genre)))

(defcommand smirk-random-tracks () ()
    "Play a random 250 tracks in MPD."
  (stumpwm:run-shell-command "smirk tracks"))

(defcommand smirk-random-album () ()
    "Play a random album in MPD."
  (stumpwm:run-shell-command "smirk album"))


;; swank
(swank-loader:init)

(defvar *swank-p* nil)

(defcommand swank () ()
"Starts a swank server on port 4005 and notifies the user."
  (setf stumpwm:*top-level-error-action* :break)
  (if *swank-p*
      (message "Swank server already running.")
    (progn
      (swank:create-server :port 4005
                           :style swank:*communication-style*
                           :dont-close t)
      (setf *swank-p* t)
      (message "Starting swank on port 4005."))))


(defun show-key-seq (key seq val)
  (declare (ignore key val))
  (stumpwm:message (print-key-seq (reverse seq))))

(add-hook stumpwm:*key-press-hook* 'show-key-seq)


;; MPD
(mpd:mpd-connect)

(setf *window-format* "<%n%m%30t>")
(setf mpd:*mpd-modeline-fmt* "%L %a - %t (%n/%p)")

;; mode-line
(defcommand toggle-current-mode-line () ()
  "Toggle the current screens mode-line."
  (stumpwm:toggle-mode-line (stumpwm:current-screen)
                            (stumpwm:current-head)))


(defun my/fmt-cpu-usage (ml)
  "Returns a string representing current the percent of average CPU
  utilization."
  (declare (ignore ml))
  (let* ((cpu (nth-value 0 (truncate (* 100 (cpu::current-cpu-usage))))))
    (format nil "~A%" cpu)))

(pushnew '(#\U my/fmt-cpu-usage) *screen-mode-line-formatters* :test 'equal)


(defun my/fmt-mem-usage (ml)
  "Returns a string representing the current percent of used memory."
  (declare (ignore ml))
  (let* ((mem (mem::mem-usage))
         (allocated (truncate (/ (nth 1 mem) 1000))))
    (format nil "~A mb" allocated)))

(pushnew '(#\N my/fmt-mem-usage) *screen-mode-line-formatters* :test 'equal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun enable-mode-line-all-heads ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(setf stumpwm:*screen-mode-line-format*
      (list
       "[^B%n^b] %W "
       "^> %m | %N | %U | "
       '(:eval (run-shell-command "date '+%F %a %R' | tr -d [:cntrl:]" t))))

(setf *mode-line-timeout* 2)

(enable-mode-line-all-heads)
