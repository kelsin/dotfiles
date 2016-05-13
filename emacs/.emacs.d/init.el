;;; init.el --- Kelsin's emacs init file

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

;; This is my personal Emacs init.el

;;; Code:

;; Fast startup (tramp bug)
(setq tramp-ssh-controlmaster-options "")

;; Many things require the cl libraries
(require 'cl)

;; System Check
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar cygwin-p (string-match "cygwin" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

;; Standard System-Independent Paths
(setq load-path (append load-path (list "~/.emacs.d/kelsin")))

(defun path-add (path)
  "Add a path instance to both exec-path and $PATH after checking
  that the directory exists"
  (if (file-directory-p path)
      (progn
        (setq exec-path (append exec-path (list path)))
        (setenv "PATH" (concat (getenv "PATH") ":" path)))))

;; Mac
(if macosx-p
    (progn
      (setq load-path (append load-path (list "/usr/local/share/emacs/site-lisp/")))
      (path-add "/usr/local/bin")))

;; Cygwin path
(if cygwin-p
    (progn
      (path-add "/bin")
      (path-add "/usr/bin")
      (path-add "/usr/local/bin")
      (path-add "~/bin")))

;; Load Packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Custom File
(setq custom-file "~/.emacs.d/kelsin/kelsin-custom.el")
(load custom-file)

;; Simple Options
(require 'kelsin-options)

;; Evil
(require 'kelsin-evil)

;; Load Other Modes
(require 'kelsin-modes)

;; Load Custom Functions
(require 'kelsin-functions)

;; Mail Helpers
;; (require 'kelsin-mail)

;; Snippets
(require 'kelsin-snippets)

;; Keybindings
(require 'kelsin-bindings)

;; Powerline
(require 'kelsin-powerline)

;; Helm
(require 'kelsin-helm)

;; Org
(require 'kelsin-org)

;; Load Theme and Font
(setq-default line-spacing 3)
(add-to-list 'default-frame-alist '(font . "Monaco-16"))
(add-to-list 'custom-theme-load-path "~/blizzard/src/blizzard-colors/emacs")
(load-theme 'blizzard 't)

;; Diminish
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'company-mode)
(diminish 'global-whitespace-mode)
(diminish 'yas-minor-mode)

;; Start up the server
(server-start)

(provide 'init)
;;; init.el ends here
