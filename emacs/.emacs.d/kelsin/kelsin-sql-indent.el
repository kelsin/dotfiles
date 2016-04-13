;;; sql-indent.el --- Indenting SQL the Kelsin way

;; Copyright (C) 2010  Christopher Giroir

;; Author: Christopher Giroir <cgiroir@berklee.edu>
;; Keywords: languages

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

;; Idents sql functions the right way

;;; Code:

(defconst sql-start-regexp
  (regexp-opt '("create"
                "delete"
                "insert"
                "select"
                "update") 'words))

(defconst sql-join-words
  '("right" "inner" "outer" "cross"))

(defconst sql-join-regexp
  (regexp-opt sql-join-words 'words))

(defconst sql-words-regexp
  (regexp-opt (append '("having" "values"
                        "where" "union" "order" "limit" "group"
                        "from" "left" "join" "then"
                        "and" "set" "or")
                      sql-join-words)
              'words))

(defconst sql-join-regexp
  "\\(?1:\\(\\(inner\\|\\(left\\|right\\|full\\)\\( outer\\)?\\|cross\\) \\)?join\\)")

(defconst sql-newline-regexp
 (regexp-opt '("from" "or" "and" "then" "union" "set" "values"
                "where" "order" "group" "having" "limit")
              'words))

(defconst sql-block-start-regexp "{\\|\"")
(defconst sql-block-end-regexp "}\\|\"\\|;")

(defun sql-narrow-to-query ()
  "Narrows the region to the current query"
  (interactive)
  (save-excursion
    (sql-move-to-beginning)
    (beginning-of-line)
    (push-mark)
    (sql-move-to-end)
    (end-of-line)
    (narrow-to-region (mark) (point))))

(defun sql-remove-newlines ()
  (save-excursion
    (save-restriction
      (sql-narrow-to-query)
      (goto-char (point-min))
      (perform-replace "\n\\|\r" " " nil t nil)
      (goto-char (point-min))
      (perform-replace "\\s-+" " " nil t nil))))

(defun sql-indent-query ()
  "Indents every line of the current query"
  (interactive)
  (save-excursion
    (save-restriction
      (sql-narrow-to-query)
      (indent-region (point-min) (point-max)))))

(defun sql-comma-replacements (num)
  "Returns the right replacement string depending on num"
  (let ((times (or num 0)))
    (append (make-list times ", ")
            '(",\n"))))

(defun sql-reformat-query (num)
  "Reformats the current query"
  (interactive "P")
  (save-excursion
    (save-restriction
      (sql-narrow-to-query)
      (sql-remove-newlines)
      (goto-char (point-min))
      (perform-replace sql-join-regexp "\n\\1" nil t nil)
      (goto-char (point-min))
      (perform-replace ",\s-*" (sql-comma-replacements num) nil t nil)
      (goto-char (point-min))
      (perform-replace sql-newline-regexp "\n\\1" nil t nil)
      (goto-char (point-min))
      (perform-replace "\\([^(]\\select" "\\1\nselect" nil t nil)
      (goto-char (point-min))
      (perform-replace "\\(insert into.*?\\)(" "\\1\n(" nil t nil)
      (goto-char (point-min))
      (delete-blank-lines)
      (whitespace-cleanup)))
  (sql-indent-query)
  (message num))

(defun sql-indent-offset ()
  "Looks at the current word and finds out what the offset should be"
  (cond ((looking-at "then") 12)
        ((or (looking-at sql-start-regexp)
             (looking-at sql-words-regexp))
         (max (- 6 (length (match-string 0))) 0))
        (t 7)))

(defun sql-move-to-beginning ()
  "Finds the start of the current query"
  (while (progn
           (let ((pos (sql-recent-paren-position)))
             (cond (pos (goto-char pos))
                   ((re-search-backward sql-start-regexp nil t)
                    (looking-back "union\n"))
                   ((re-search-backward "{\\|\"" nil 'goto) nil))))))

(defun sql-move-to-end ()
  "Move to the end of the current sql query"
  (re-search-forward sql-block-end-regexp nil 'end))

(defun sql-recent-paren-position ()
  "Finds the position of the next paren"
  (nth 1 (syntax-ppss)))

(defun sql-mode-clause-info ()
  "Returns information about the current location"
  (save-excursion
    (let ((pos (nth 1 (syntax-ppss))))
      (if pos
          (progn
            (goto-char (1+ pos))
            (cons (current-column)
                  (not (looking-at sql-start-regexp))))
        (cons (progn
                (while (and (re-search-backward sql-start-regexp nil t)
                            (looking-back "(")))
                (if (looking-at sql-start-regexp)
                    (current-column)
                  0))
              nil)))))

(defun sql-mode-indent ()
  "Indents a line of sql"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let* ((data (sql-mode-clause-info))
           (column (car data))
           (clause? (cdr data)))
      (indent-line-to (+ column
                         (if clause?
                             0
                           (sql-indent-offset)))))))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq indent-line-function 'sql-mode-indent)))

(provide 'sql-indent)
;;; sql-indent.el ends here
