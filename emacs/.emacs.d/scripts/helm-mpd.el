;;; helm-mpd.el

(require 'cl)
(require 'helm)

(defun helm-mpd-new-queue ()
  "Creates a new empty queue."
  (cons nil nil))

(defun helm-mpd-queue-enqueue (queue x)
  "Adds X to the end of a queue QUEUE."
  (let ((node (cons x nil)))
    (if (car queue)
        (setf (cdr (cdr queue)) node
              (cdr queue) node)
      (setf (car queue) node
            (cdr queue) node))))

(defun helm-mpd-queue-dequeue (queue)
  "Removes the first element of QUEUE. Returns nil if the queue is empty."
  (let ((v (car (car queue))))
    (if (eq (car queue) (cdr queue))
        (setf (car queue) nil
              (cdr queue) nil)
      (pop (car queue)))
    v))

(defvar helm-mpd-answer-queue (helm-mpd-new-queue)
  "Queue that holds the lines that have been received from MPD.")

(defvar helm-mpd-received-text ""
  "String containing the text sent by MPD.
Its content gets split into lines, passed  into `helm-mpd-answer-queue' and
progressively filtered.")

(defun helm-mpd-receive (text)
  "Handles data received from MPD.
The text is split into lines and parsed by  `helm-mpd-parse-line'."
  (setq helm-mpd-received-text (concat helm-mpd-received-text text))
  (loop for eol = (position ?\n helm-mpd-received-text)
        while eol do
    (let ((cur  (subseq helm-mpd-received-text 0 (1+ eol)))
          (rest (subseq helm-mpd-received-text (1+ eol))))
      (unwind-protect
          (helm-mpd-parse-line cur))
        (setq helm-mpd-received-text rest))))

(defstruct helm-mpd-song
  filename time artist title id)

(defun helm-mpd-song-format (song)
  "Generates a string to describe a song."
  (let* ((time (helm-mpd-song-time song)))
      (format  "%s - %s [%2d:%2d]"
               (or (helm-mpd-song-artist song) "?")
               (or (helm-mpd-song-title song)
                   (helm-mpd-song-filename song)
                   "?")
               (if time (/ time 60) "?")
               (if time (mod time 60) "?"))))

(defvar helm-mpd-song-queue (helm-mpd-new-queue))
(defvar helm-mpd-current-song nil)

(defun helm-mpd-push-current-song ()
  "Resets `helm-mpd-current-song' to nil and enqueues its current value to
`helm-mpd-song-queue'."
  (when helm-mpd-current-song
    (helm-mpd-queue-enqueue helm-mpd-song-queue
                            helm-mpd-current-song)
    (setq helm-mpd-current-song nil)))

(defvar helm-mpd-received-list nil
  "Variable set to non-nil once the entire playlist has been received.")

(defun helm-mpd-parse-line (line)
  "Parses a line received from MPD.
It handles errors and OK lines. Other lines are assumed to be \"key: value\"
pairs containing information about songs."
  (cond
   ((string-match "^OK" line)
    (if helm-mpd-current-song (setq helm-mpd-received-list t))
    (helm-mpd-push-current-song))
   ((string-match "^ACK \\[\\([0-9]+\\)@\\([0-9]+\\)\\] {\\([0-9]+\\)} (.+)\n"
                  line)
    (message "helm-mpd: error %d: %s"
             (match-string 1 line) (match-string 4 line)))
   ((string-match "^\\(.+?\\): \\(.*\\)\n" line)
    (let ((field (match-string 1 line))
          (value (match-string 2 line)))
      (cond
       ((string= field "file")
        (helm-mpd-push-current-song)
        (setq helm-mpd-current-song (make-helm-mpd-song :filename value)))
       ((string= field "Time")
        (setf (helm-mpd-song-time helm-mpd-current-song) (string-to-number value)))
       ((string= field "Artist")
        (setf (helm-mpd-song-artist helm-mpd-current-song) value))
       ((string= field "Id") (setf (helm-mpd-song-id helm-mpd-current-song)
                                   (string-to-number value)))
       ((string= field "Title")
        (setf (helm-mpd-song-title helm-mpd-current-song) value)))))))

(defvar helm-mpd-process nil
  "Network connection to MPD.")

(defvar helm-mpd-host "localhost"
  "Host used to connect to MPD.")
(defvar helm-mpd-port 6600
  "Port used to connect to MPD.")

(defun helm-mpd-open ()
  "Connects to MPD."
  (when helm-mpd-process
    (delete-process helm-mpd-process)
    (setq helm-mpd-current-song nil
          helm-mpd-song-queue (helm-mpd-new-queue)
          helm-mpd-received-text ""
          helm-mpd-answer-queue (helm-mpd-new-queue)
          helm-mpd-received-list nil))
  (setq helm-mpd-process
        (open-network-stream "Helm MPD"
                             (get-buffer-create "*mpd*")
                             helm-mpd-host helm-mpd-port))
  (set-process-filter helm-mpd-process
                      (lambda (process text) (helm-mpd-receive text))))

(defun helm-mpd-request-song-list ()
  "Requests a list of all songs in the current playlist from MPD."
  (process-send-string helm-mpd-process "playlistinfo\n"))

(defun helm-mpd-song-play (song)
  "Starts playing SONG in MPD."
  (process-send-string helm-mpd-process
                       (format "playid %d\n" (helm-mpd-song-id song))))

(defvar helm-source-mpd-songs
  '((name . "MPD Songs")
    (init . (lambda ()
              (helm-mpd-open)
              (helm-mpd-request-song-list)
              (while (not helm-mpd-received-list)
                (accept-process-output helm-mpd-process))))
    (candidates . (lambda ()
                    (mapcar (lambda (song)
                              (cons (helm-mpd-song-format song)
                                    song))
                            (car helm-mpd-song-queue))))
    (action . (("Play" . helm-mpd-song-play))))
  "A source that requests all the song and allows you to select one of them to
be played.")

(defun mpd ()
  "Starts helm with `helm-source-mpd-songs'."
  (interactive)
  (helm :sources '(helm-source-mpd-songs) :prompt "Song: "))

(provide 'helm-mpd)
