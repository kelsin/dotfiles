;;; kelsin-mpd.el --- MPD Interface

;; Copyright (C) 2008  

;; Author:  <cgiroir@berklee.edu>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'custom)
(require 'libmpdee)

;; Customizations
(defgroup kelsin-mpd nil
  "Main options concerning the kelsin-mpd interface"
  :group 'applications)

(defcustom kelsin-mpd-host "localhost"
  "Host running mpd"
  :type 'string
  :group 'kelsin-mpd)

(defcustom kelsin-mpd-port 6600
  "Port to connect to"
  :type 'integer
  :group 'kelsin-mpd)

(defcustom kelsin-mpd-thumb-extension "png"
  "Default image type (extension) to use. This value can be
  anything that imagemagick's convert can handle."
  :type 'string
  :group 'kelsin-mpd)

(defcustom kelsin-mpd-music-dir "/var/lib/mpd/music"
  "Directory on your local filesystem where your mpd music files
reside. This can be a share as long as we can use the normal
emacs file functions on it. This is currently only used to find
album covers"
  :type 'directory
  :group 'kelsin-mpd)

(defcustom kelsin-mpd-thumb-dir "~/.emacs.d/.kelsin-mpd-thumbs"
  "Directory to put thumbnails of album covers into. Make sure to
  clear this directory out if you change this setting."
  :type 'directory
  :group 'kelsin-mpd)

;; Mode functions
(define-derived-mode kelsin-mpd-mode
  nil "Kelsin MPD Mode"
  "Main mode for browsing kelsin-mpd buffers")

(define-key kelsin-mpd-mode-map "q"
  (lambda ()
    (interactive)
    (kill-buffer nil)))
(define-key kelsin-mpd-mode-map "r"
  (lambda ()
    (interactive)
    (mpd-toggle-repeat (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map "z"
  (lambda ()
    (interactive)
    (mpd-toggle-random (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map "Z"
  (lambda ()
    (interactive)
    (mpd-shuffle-playlist (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map ">"
  (lambda ()
    (interactive)
    (mpd-next (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map "<"
  (lambda ()
    (interactive)
    (mpd-prev (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map "s"
  (lambda ()
    (interactive)
    (mpd-stop (kelsin-mpd-connection))))
(define-key kelsin-mpd-mode-map (kbd "\C-c p") 'kelsin-mpd-clear-playlist)
(define-key kelsin-mpd-mode-map "p" 'kelsin-mpd-play-or-pause)
(define-key kelsin-mpd-mode-map (kbd "\C-c f") 'kelsin-mpd-fill-playlist)

(global-set-key "\C-cmn" 'kelsin-mpd-now-playing)
(global-set-key "\C-cmb" 'kelsin-mpd-browse-albums)

;; Interface functions
(defun kelsin-mpd-clear-playlist ()
  (interactive)
  (mpd-clear-playlist (kelsin-mpd-connection))
  (kelsin-mpd-update-now-playing))

(defun kelsin-mpd-play-or-pause ()
  (interactive)
  (if (eq 'stop (kelsin-mpd-state))
      (mpd-play (kelsin-mpd-connection) 0)
    (mpd-pause (kelsin-mpd-connection))))

(defun kelsin-mpd-now-playing ()
  (interactive)
  (kelsin-mpd-insert-now-playing-timer)
  (with-current-buffer (get-buffer-create "*kelsin-mpd-now-playing*")
    (kelsin-mpd-mode)
    (add-hook 'kill-buffer-hook
              (function (lambda ()
                          (if kelsin-mpd-now-playing-timer
                              (cancel-timer kelsin-mpd-now-playing-timer))))))
  (set-window-buffer (selected-window) "*kelsin-mpd-now-playing*"))

(defun kelsin-mpd-browse-albums ()
  (interactive)
  (let ((buffer (get-buffer-create "*kelsin-mpd-browse-albums*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((last-album nil))
        (dolist (song (mpd-get-songs (kelsin-mpd-connection) "listallinfo"))
          (let ((current-album (plist-get song 'Album)))
            (if (not (string= last-album current-album))
                (progn
                  (setq last-album current-album)
                  (kelsin-mpd-display-album-art song)
                  (insert-text-button (concat (plist-get song 'Artist)
                                              " - "
                                              (plist-get song 'Album))
                                      'action 'kelsin-mpd-add-album-to-playlist
                                      'album (plist-get song 'Album)
                                      'help-echo "Add this album to the playlist"
                                      'follow-link t)
                  (insert "\n"))))))
      (kelsin-mpd-mode)
      (set-window-buffer (selected-window) "*kelsin-mpd-browse-albums*")
      (setq buffer-read-only t))))

(defun kelsin-mpd-fill-playlist ()
  (interactive)
  (mpd-clear-playlist (kelsin-mpd-connection))
  (mpd-enqueue (kelsin-mpd-connection)
               (mapcar (lambda (song)
                         (plist-get song 'file))
                       (mpd-get-songs (kelsin-mpd-connection) "listall")))
  (kelsin-mpd-update-now-playing))

;; Low Level Functions
(setq *kelsin-mpd-connection* nil)
(defun kelsin-mpd-connection ()
  (if (not *kelsin-mpd-connection*)
      (setq *kelsin-mpd-connection* (mpd-conn-new kelsin-mpd-host kelsin-mpd-port 0 nil))
    *kelsin-mpd-connection*))

(defun kelsin-mpd-add-album-to-playlist (button)
  (dolist (song (mpd-search (kelsin-mpd-connection)
                            'album
                            (get-text-property (marker-position button)
                                               'album)))
    (mpd-enqueue (kelsin-mpd-connection)
                 (plist-get song 'file)))
  (kill-buffer nil)
  (kelsin-mpd-now-playing))

(defun kelsin-mpd-playing-p ()
  (eq 'play
      (kelsin-mpd-state)))

(defun kelsin-mpd-state ()
  (plist-get (mpd-get-status (kelsin-mpd-connection))
             'state))

(defun kelsin-mpd-insert-now-playing-timer ()
  (kelsin-mpd-insert-now-playing)
  (if (get-buffer "*kelsin-mpd-connection*")
      (with-current-buffer (get-buffer-create "*kelsin-mpd-now-playing*")
        (make-variable-buffer-local 'kelsin-mpd-now-playing-timer)
        (setq kelsin-mpd-now-playing-timer (run-at-time "5 sec" nil
                                                        'kelsin-mpd-insert-now-playing-timer)))))

(defun kelsin-mpd-insert-now-playing ()
  (let ((buffer (get-buffer-create "*kelsin-mpd-now-playing*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((state (kelsin-mpd-state)))
        (cond ((eq state 'play)
               (let ((song (mpd-get-current-song (kelsin-mpd-connection))))
                 (kelsin-mpd-display-album-art song)
                 (kelsin-mpd-display-song song)
                 (insert "\n")))
              ((eq state 'pause)
               (insert "Paused\n"))
              (t (insert "Stopped\n")))
        (insert "\n")
        (dolist (song (mpd-get-songs (kelsin-mpd-connection) "playlistinfo"))
          (insert-text-button (concat (plist-get song 'Artist)
                                      " - "
                                      (plist-get song 'Album)
                                      " - "
                                      (plist-get song 'Title))
                              'action 'kelsin-mpd-play-song
                              'follow-link t
                              'help-echo "Play song"
                              'song-id (plist-get song 'Id))
          (insert "\n"))
        (beginning-of-buffer)
        (setq buffer-read-only t)))))

(defun kelsin-mpd-display-song (song)
  (insert (concat (plist-get song 'Title) " by "
                  (plist-get song 'Artist) " on "
                  (plist-get song 'Album)))
  (let ((date (plist-get song 'Date)))
    (if (and date (string= "" date))
        (insert (concat " - " date)))))

(defun kelsin-mpd-thumbnail-filename (song)
  (concat (file-name-as-directory kelsin-mpd-thumb-dir)
          (plist-get song 'Artist)
          "-"
          (plist-get song 'Album)
          "."
          kelsin-mpd-thumb-extension))

(defun kelsin-mpd-album-art-filename (song)
  (nth 0 (directory-files (file-name-directory (concat (file-name-as-directory kelsin-mpd-music-dir)
                                                       (plist-get song 'file)))
                          t
                          "\\(gif\\|jpg\\|jpeg\\|png\\)$")))

(defun kelsin-mpd-display-album-art (song)
  (if (and (display-images-p)
           (file-directory-p kelsin-mpd-music-dir))
      (let ((thumbnail-filename (kelsin-mpd-thumbnail-filename song))
            (album-art-filename (kelsin-mpd-album-art-filename song)))
        (if album-art-filename
            (progn
              (if (not (file-exists-p thumbnail-filename))
                  (progn
                    (if (not (file-directory-p kelsin-mpd-thumb-dir))
                        (make-directory kelsin-mpd-thumb-dir t))
                    (call-process "/usr/bin/convert" nil nil nil
                                  "-sample" "100x100" (expand-file-name album-art-filename)
                                  (concat kelsin-mpd-thumb-extension ":" (expand-file-name thumbnail-filename)))))
              (let ((image (create-image thumbnail-filename nil nil)))
                (insert-image (create-image thumbnail-filename nil nil) (plist-get song 'Album))
                (insert " ")))))))

(defun kelsin-mpd-play-song (button)
  (mpd-play (kelsin-mpd-connection)
            (get-text-property (marker-position button)
                               'song-id)
            t)
  (kelsin-mpd-update-now-playing))

(defun kelsin-mpd-update-now-playing ()
  (save-excursion
    (kelsin-mpd-insert-now-playing)))

(provide 'kelsin-mpd)
;;; kelsin-mpd.el ends here
