;;; kelsin-org.el --- Custom Org-Mode settings

;; Copyright (C) 2008  Christopher Giroir

;; Author: Christopher Giroir <cgiroir@berklee.edu>
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

;; Turn on remember mode
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/todo.org")
(setq org-log-done t)
(setq org-startup-folded nil)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-capture-templates
      '(("c" "Code" entry (file "~/Dropbox/org/code.org") "* %?\n\n  %a")
        ("s" "Song" entry (file "~/Dropbox/org/songs/notes.org") "* %U\n  %?")
        ("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Quick") "* TODO %?\n  %i")))

(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(provide 'kelsin-org)
;;; kelsin-org.el ends here
