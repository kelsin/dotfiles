;;; kelsin-functions.el --- Functions I wrote

;; Copyright (C) 2007 Christopher Giroir

;; Author: Christopher Giroir <kelsin@valefor.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:
(cond (macosx-p
        (defun system-open (item)
          "Opens an item with open"
          (call-process "/usr/bin/env" nil nil nil
            "open"
            item)))
  (linux-p
    (defun system-open (item)
      "Opens an item with gnome-open"
      (call-process "/usr/bin/env" nil nil nil
        "gnome-open"
        item)))
  (mswindows-p
    (defun system-open () '())))

(defun system-open-buffer ()
  "Opens the current buffer's file with open"
  (interactive)
  (if (buffer-file-name)
    (system-open (buffer-file-name))))

;; Ido Tag Searching
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
      tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))
(global-set-key [(control meta .)] 'ido-find-tag)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((files (mapcar #'(lambda (file)
                            (cons (file-name-nondirectory file) file))
                  recentf-list))
          (file (cdr (assoc (ido-completing-read "Choose recent file: "
                              (mapcar 'car files)
                              nil t)
                       files))))
    (when file
      (find-file file))))

;; Ido Bookmarks
(autoload 'bookmark-all-names "bookmark")
(defun bookmark-ido-find-file ()
  "Find a bookmark using Ido."
  (interactive)
  (let ((bm (ido-completing-read "Choose bookmark: "
              (bookmark-all-names)
              nil t)))
    (when bm
      (bookmark-jump bm))))

(defun kelsin-find-tags-file (&optional path)
  "Recursively searches for a TAGS (or tags) file from the current path"
  (let ((current-file (or path (buffer-file-name))))
    (if current-file
      (let* ((parent (file-name-directory current-file))
              (etags (concat parent "TAGS"))
              (ctags (concat parent "tags")))
        (cond
          ((equal parent path) nil)
          ((file-exists-p etags) etags)
          ((file-exists-p ctags) ctags)
          (t (kelsin-find-tags-file (directory-file-name parent))))))))

(add-hook 'find-file-hook
  '(lambda ()
     (let (tags-file (kelsin-find-tags-file))
       (if tags-file
         (progn
           (visit-tags-table tags-file t)
           (message (concat "Setting tags table for" (buffer-name) " to " tags-file)))))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun reindent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) -999)
  (indent-buffer))

(defun reformat-buffer ()
  "Runs whitespace-cleanup and then indents the whole buffer"
  (interactive)
  (whitespace-cleanup)
  (reindent-buffer))

(defun close-all-buffers ()
  "Close all open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))

(fset 'fix-bad-xml-attributes
  [?\C-\M-s ?[ ?^ ?% ?] ?= ?[ ?^ ?\" ?] ?\C-m left ?\" ?\C-\M-s ?  ?\\ ?| ?> ?\C-m left ?\"])

;; Byte compile the current file automatically
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
          (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'byte-compile-current-buffer)

;; Status report stuff
(defun get-work-week-times (days)
  (let* ((ct (time-subtract (current-time)
               (days-to-time days)))
          (dow (nth 6 (decode-time ct)))
          (monday (time-subtract ct
                    (days-to-time (- dow 1))))
          (friday (time-subtract ct
                    (days-to-time (- dow 5)))))
    (list monday friday)))

(defun insert-latex-status-report (days)
  (message "Loading basecamp data ...")
  (let* ((times (get-work-week-times days))
          (monday (format-time-string "%Y%m%d"
                    (nth 0 times)))
          (friday (format-time-string "%Y%m%d"
                    (nth 1 times))))
    (insert (shell-command-to-string (concat "/home/cgiroir/share/berklee/bin/gettime.rb "
                                       monday
                                       " "
                                       friday)))))

(defun insert-latex-status-report-date (days)
  (let* ((times (get-work-week-times days))
          (monday (format-time-string "%m-%d-%Y"
                    (nth 0 times)))
          (friday (format-time-string "%m-%d-%Y"
                    (nth 1 times))))
    (insert (concat monday
              " to "
              friday))))

(defun generate-new-status-report (days)
  (insert "\\documentclass[letterpaper,12pt]{article}\n\\usepackage{fullpage}\n\\usepackage{bookman}\n\n")
  (insert "\\setlength{\\parindent}{0pt}\n\n")
  (insert "\\begin{document}\n\n")
  (insert "{\\Huge Status Report} \\\\\n")
  (insert "{\\Large Christopher Giroir} \\\\\n")
  (insert-latex-status-report-date days)
  (insert "\n\n")
  (insert-latex-status-report days)
  (insert "% Start Custom\n\n\n\n% End Custom\n\n")
  (insert "\\end{document}\n")
  (forward-line -5))

(defun create-status-report (filename days)
  (switch-to-buffer (generate-new-buffer "status-report"))
  (generate-new-status-report days)
  (write-file filename))

(defun mark-custom-status-report ()
  (beginning-of-buffer)
  (search-forward "% Start Custom")
  (beginning-of-line)
  (set-mark (point))
  (search-forward "% End Custom"))

(defun generate-status-report (days)
  (interactive "p")
  (let* ((days (if (eq current-prefix-arg nil)
                 0
                 days))
          (times (get-work-week-times days))
          (monday (format-time-string "%Y%m%d"
                    (nth 0 times)))
          (friday (format-time-string "%Y%m%d"
                    (nth 1 times)))
          (dirname "/home/cgiroir/share/berklee/status/")
          (filename (concat dirname "/" friday ".tex")))
    (make-directory dirname t)
    (if (file-exists-p filename)
      (progn
        (message "Status report already exists, saving custom section")
        (find-file filename)
        (save-buffer)
        (mark-custom-status-report)
        (let ((custom (delete-and-extract-region (region-beginning) (region-end))))
          (not-modified)
          (kill-buffer (current-buffer))
          (create-status-report filename days)
          (mark-custom-status-report)
          (delete-region (region-beginning) (region-end))
          (insert custom)
          (save-buffer)))
      (create-status-report filename days))
    (TeX-PDF-mode t)
    (TeX-command-master nil)))

;; Indenting for js2
;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(defun kelsin/js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
            (parse-status (save-excursion (syntax-ppss (point-at-bol))))
            (offset (- (current-column) (current-indentation)))
            (indentation (espresso--proper-indentation parse-status))
            node)
      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

;; http://www.emacswiki.org/emacs/ImenuMode#toc10
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
    ((not symbol-list)
      (let ((ido-mode ido-mode)
             (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                 ido-enable-flex-matching t))
             name-and-pos symbol-names position)
        (unless ido-mode
          (ido-mode 1)
          (setq ido-enable-flex-matching t))
        (while (progn
                 (imenu--cleanup)
                 (setq imenu--index-alist nil)
                 (ido-goto-symbol (imenu--make-index-alist))
                 (setq selected-symbol
                   (ido-completing-read "Symbol? " symbol-names))
                 (string= (car imenu--rescan-item) selected-symbol)))
        (unless (and (boundp 'mark-active) mark-active)
          (push-mark nil t nil))
        (setq position (cdr (assoc selected-symbol name-and-pos)))
        (cond
          ((overlayp position)
            (goto-char (overlay-start position)))
          (t
            (goto-char position)))))
    ((listp symbol-list)
      (dolist (symbol symbol-list)
        (let (name position)
          (cond
            ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
            ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
            ((stringp symbol)
              (setq name symbol)
              (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
          (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
            (add-to-list 'symbol-names name)
            (add-to-list 'name-and-pos (cons name position))))))))

(defun kelsin/replace-smart-quotes (beg end)
  "Replace ’ (the curly typographical quote, unicode hexa 2019) to ' (ordinary ascii quote)."
  (interactive "r")
  (save-excursion
    (format-replace-strings '(("\x2019" . "'")) nil beg end)))

;; Lookup word in google
;; http://ergoemacs.org/emacs/emacs_lookup_ref.html
(defun kelsin/google ()
  "Look up the word under cursor in Google."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
      (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'symbol)))
    (setq myWord (replace-regexp-in-string " " "_" myWord))
    (setq myUrl (concat "https://www.google.com/#q=" myWord))
    (browse-url myUrl)))

;; emacs doesn't actually save undo history with revert-buffer
;; see http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-04/msg00151.html
;; fix that.
(defun revert-buffer-keep-history (&optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
  (interactive)

  ;; tell Emacs the modtime is fine, so we can edit the buffer
  (clear-visited-file-modtime)

  ;; insert the current contents of the file on disk
  (widen)
  (delete-region (point-min) (point-max))
  (insert-file-contents (buffer-file-name))

  ;; mark the buffer as not modified
  (not-modified)
  (set-visited-file-modtime))
(setq revert-buffer-function 'revert-buffer-keep-history)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'kelsin-functions)
;;; kelsin-functions.el ends here
