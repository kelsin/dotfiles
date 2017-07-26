;;; kelsin-bindings.el --- My Custom Keybindings

;; Copyright (C) 2014  Christopher Giroir

;; Author: Christopher Giroir <cgiroir@berklee.edu>
;; Keywords: convenience

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

;; Fancy Kill Ring
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; Reindent then newline and indent
(global-set-key (kbd "M-RET") 'reindent-then-newline-and-indent)

;; Bind fixup-whitespace
(global-set-key "\C-xw" 'fixup-whitespace)

;; Ido
(global-set-key (kbd "M-i") 'ido-goto-symbol)
; (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-x r b") 'bookmark-ido-find-file)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Avy Goto Work
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)

;; Function keybindings
(global-set-key [f1] 'info)
(global-set-key [f2] 'kelsin/google)
(global-set-key [f5] 'javadoc-lookup)
(global-set-key [(shift f5)] 'javadoc-help)
;;; (global-set-key [f8] 'reformat-buffer)

(use-package prettier-js
  :ensure t
  :bind ("<f8>" . prettier-js))

;; No suspend in terminal
(global-unset-key (kbd "C-z"))

(provide 'kelsin-bindings)
;;; kelsin-bindings.el ends here
