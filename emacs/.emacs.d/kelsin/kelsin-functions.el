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
(cond ((eq system-type 'darwin)
          (defun system-open (item)
              "Opens an item with open"
              (call-process "/usr/bin/env" nil nil nil
                  "open"
                  item)))
    ((or (eq system-type 'windows-nt)
         (eq system-type 'cygwin))
        (defun system-open (item)
            "Opens an item with start"
            (call-process "/usr/bin/env" nil nil nil
                "start"
                item)))
    (defun system-open (item)
        "Opens an item with gnome-open"
        (call-process "/usr/bin/env" nil nil nil
            "gnome-open"
            item)))

(defun system-open-buffer ()
    "Opens the current buffer's file with open."
    (interactive)
    (if (buffer-file-name)
        (system-open (buffer-file-name))))

(defun kelsin-find-tags-file (&optional path)
    "Recursively search for a TAGS (or tags) file from the current or passed in PATH."
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
    "Run the default indent function on the whole buffer."
    (interactive)
    (indent-region (point-min) (point-max) nil))

(defun reformat-buffer ()
    "Run \"whitespace-cleanup\" and then reindent the whole buffer."
    (interactive)
    (command-execute 'whitespace-cleanup)
    (command-execute 'indent-buffer))

(defun close-all-buffers ()
    "Close all open buffers."
    (interactive)
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows))

;; Indenting for js2
;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(defun kelsin/js2-indent-function ()
    "Indent the current line by using espresso's indentation function."
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
    "Revert the current buffer with a undo history addition."
    (interactive)

    ;; tell Emacs the modtime is fine, so we can edit the buffer
    (clear-visited-file-modtime)

    ;; insert the current contents of the file on disk
    (widen)
    (delete-region (point-min) (point-max))
    (insert-file-contents (buffer-file-name))

    ;; mark the buffer as not modified
    (set-buffer-modified-p nil)
    (set-visited-file-modtime))
(setq revert-buffer-function 'revert-buffer-keep-history)

(defun what-face (pos)
    "Show the current face at POS in the modeline."
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
        (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun kelsin/devdocs ()
    "Look up the word under cursor in devdocs.io"
    (interactive)
    (let ((url "http://devdocs.io/#q=")
             (query (or (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                            (thing-at-point 'symbol))
                        "")))
        (browse-url (concat url query))))

(provide 'kelsin-functions)
;;; kelsin-functions.el ends here
