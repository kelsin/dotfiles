;;; kelsin-mmmm.el --- Custom installed mmm modes

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file loads up my custom MMM modes

;;; Code:

(require 'mmm-mode)
(require 'mmm-auto)

;; Turn on mmm-mode when you come across files you know about
(setq mmm-global-mode 'maybe)

;; Set decoration level
(setq mmm-submode-decoration-level 2)

;; ACS Section
(mmm-add-group 'oacs-tcl
               '((oacs-sql-brackets
                  :submode sql-mode
                  :front "db_\\(foreach\\|list\\|1row\\|multirow\\|0or1row\\|dml\\|string\\).*\\(-extend {.*}\\)?{"
                  :back "}")
                 (oacs-sql-quotes
                  :submode sql-mode
                  :front "db_\\(foreach\\|list\\|1row\\|multirow\\|0or1row\\|dml\\|string\\).*\\(-extend {.*}\\)?\\\""
                  :back "^[^[
]*\\\""
                  :include-back t
                  :back-offset -1)))

(mmm-add-group 'oacs-adp
               '((oacs-adp-if
                  :submode tcl-mode
                  :front "<if\\s-*"
                  :front-offset 0
                  :back "\\s-*>")
                 (oacs-adp-tcl
                  :submode tcl-mode
                  :font "<%=?"
                  :back "%>")))

(mmm-add-mode-ext-class nil "\\.tcl$" 'oacs-tcl)
(mmm-add-mode-ext-class nil "\\.adp$" 'oacs-adp)

;; Erb

(mmm-add-group 'erb-mmm
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%-"  . mmm-code-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=-]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))
   (erb-script
    :submode js2-mode
    :front "<script[^>]*>"
    :back "</script>")))
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))
(add-to-list 'auto-mode-alist '("\.erb$" . nxml-mode))

(provide 'kelsin-mmm)
;;; kelsin-modes.el ends here
