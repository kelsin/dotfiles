;;; kelsin-java.el --- Functions to help with java programming

;; Copyright (C) 2014  Christopher Giroir

;; Author: Christopher Giroir <cgiroir@berklee.edu>
;; Keywords: tools, languages, files

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

;; Exec Path
(path-add "~/blizzard/src/web-dev-tools/maven/bin")
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.7.0_67.jdk/Contents/Home")

;; Imports
(defvar kelsin/import-regexp "^import\\( static\\)? \\([A-Za-z1-0]*\\)\\.")
(defvar kelsin/import-prefixes (nconc (delete "" (split-string "java;javax;org;com;" ";")) (list "other")))

(defun kelsin/organize-imports ()
  "Resort your java imports at the top of the file."
  (interactive)
  (let ((imports (kelsin/group-imports)))
    (kelsin/clear-imports)
    (dolist (group imports)
      (princ "\n" (current-buffer))
      (dolist (import group)
        (princ import (current-buffer))
        (princ "\n" (current-buffer))))
    (kelsin/goto-first-import)))

(defun kelsin/clear-imports ()
  "Remove all import lines."
  (let ((start (kelsin/goto-first-import)))
    (goto-char (point-min))
    (while (re-search-forward kelsin/import-regexp nil 't)
      (delete-region (line-beginning-position) (line-end-position))
      (goto-char (point-min)))
    (goto-char start)
    (delete-blank-lines)))

(defun kelsin/goto-first-import ()
  "Move the cursor to the start of the first java import line."
  (interactive)
  (goto-char (point-min))
  (re-search-forward kelsin/import-regexp nil 't)
  (beginning-of-line)
  (point))


(defun kelsin/next-import ()
  "Find the next import statement in the current file."
  (if (re-search-forward kelsin/import-regexp nil t)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun kelsin/collect-imports ()
  "Collects all import lines from the current buffer into a list."
  (save-excursion
    (goto-char (point-min))
    (let (imports next)
      (setq imports '())
      (setq next (kelsin/next-import))
      (while next
        (add-to-list 'imports next)
        (setq next (kelsin/next-import)))
      imports)))

(defun kelsin/group-imports ()
  "Group imports by static/non-static and then be prefixes."
  (let ((imports (kelsin/collect-imports)))
    (kelsin/group-imports-by-static imports '() '())))

(defun kelsin/group-imports-by-static (imports static non-static)
  "Group IMPORTS into a STATIC list and a NON-STATIC list."
  (let ((import (car imports)))
    (if import
        (progn
          (string-match kelsin/import-regexp import)
          (if (match-string 1 import)
              (kelsin/group-imports-by-static (cdr imports) (cons import static) non-static)
            (kelsin/group-imports-by-static (cdr imports) static (cons import non-static))))
      (delete nil
              (append (kelsin/group-imports-by-prefix static) (kelsin/group-imports-by-prefix non-static))))))

(defun kelsin/group-imports-by-prefix-with-table (imports table)
  "Group IMPORTS into lists based on prefix and collect them into a hash TABLE."
  (let ((import (car imports)))
    (if import
        (progn
          (string-match kelsin/import-regexp import)
          (let ((prefix (match-string 2 import)))
            (let ((key (if (member prefix kelsin/import-prefixes)
                           prefix
                         "other")))
              (let ((value (gethash key table '())))
                (puthash key (cons import value) table)
                (kelsin/group-imports-by-prefix-with-table (cdr imports) table)))))
      table)))

(defun kelsin/group-imports-by-prefix (imports)
  "Group IMPORTS into lists based on prefix."
  (let ((table (kelsin/group-imports-by-prefix-with-table imports (make-hash-table :test 'equal))))
    (nconc (delete nil
                   (mapcar (lambda (prefix)
                             (sort (gethash prefix table '())
                                   'string<))
                           kelsin/import-prefixes))
           (delete nil
                   (sort (gethash "other" table '())
                         'string<)))))

(defun kelsin/java-format ()
  "Use the eclipse java formatter on the buffer."
  (interactive)
  (let ((p (point)))
    (whitespace-cleanup)
    (save-buffer)
    (message "Running eclipse code formatter ...")
    (call-process "c:/Program Files/eclipse/eclipse.exe"
                  nil
                  nil
                  nil
                  "-nosplash"
                  "-vm"
                  "c:/blizzard/opt/jdk1.7.0_45/bin/javaw.exe"
                  "-application"
                  "org.eclipse.jdt.core.JavaCodeFormatter"
                  "-config"
                  "c:/blizzard/git/configs/org.eclipse.jdt.core.prefs"
                  (buffer-file-name))
    (revert-buffer 't 't)
    (goto-char p)
    (message "Done")))

(defun kelsin/java/switch-main-to-test (filename)
  "Switches /src/main/ to /src/test/ and appends 'Test' to the filename given"
  (if (kelsin/java/main-file-p filename)
      (replace-regexp-in-string "/src/main/"
                                "/src/test/"
                                (replace-regexp-in-string "\\.java$"
                                                          "Test.java"
                                                          filename))
    filename))

(defun kelsin/java/switch-test-to-main (filename)
  "Switches /src/test/ to /src/main/ and removes 'Test' to the filename given"
  (if (kelsin/java/test-file-p filename)
      (replace-regexp-in-string "/src/test/"
                                "/src/main/"
                                (replace-regexp-in-string "Test\\.java$"
                                                          ".java"
                                                          filename))
    filename))

(defun kelsin/java/main-file-p (filename)
  "Returns true if this file is a non-test mvn java file"
  (and (string-match "\\.java$" filename)
       (not (string-match "Test\\.java$" filename))
       (string-match "/src/main/" filename)))

(defun kelsin/java/test-file-p (filename)
  "Returns true if this file is a non-test mvn java file"
  (and (string-match "Test\\.java$" filename)
       (string-match "/src/test/" filename)))

(defun kelsin/java/toggle-main-to-test ()
  "Switches from a .java file to it's test"
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond ((kelsin/java/test-file-p filename) (kelsin/create-directory-and-find-file (kelsin/java/switch-test-to-main filename)))
          ((kelsin/java/main-file-p filename) (kelsin/create-directory-and-find-file (kelsin/java/switch-main-to-test filename)))
          ('t (message "Current file isn't a mvn .java file")))))

(defun kelsin/create-directory-and-find-file (filename)
  "Creates the needed directory and finds the file listed"
  (progn
    (make-directory (file-name-directory filename) 't)
    (find-file filename)))

;; Flycheck
(require 'flycheck)
(flycheck-define-checker java-checkstyle
  "A Java checker using checkstyle"
  :command ("checkstyle" source)
  :error-patterns
  ((warning ":" line ":" (optional column ":") " " (message) line-end))
  :modes java-mode)

;; Java Mode Hook
(add-hook 'java-mode-hook (lambda ()
                            ;; (local-set-key (kbd "<f8>") 'kelsin/java-format)
                            (ggtags-mode 1)
                            (local-set-key (kbd "C-c k t") 'kelsin/java/toggle-main-to-test)
                            (local-set-key (kbd "C-c k o") 'kelsin/organize-imports)
                            (local-set-key (kbd "C-c k i") 'kelsin/goto-first-import)
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;; (add-hook 'malabar-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))

;; Eclim
;; (setq eclim-eclipse-dirs '("~/src/eclipse"))
;; (setq eclim-executable "/Applications/eclipse/eclim")
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
(global-company-mode t)

(provide 'kelsin-java)
;;; kelsin-java.el ends here
