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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/kelsin/")

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

;; Modeline
(require 'kelsin-modeline)

;; Org
(require 'kelsin-org)

;; Load Theme and Font
(setq-default line-spacing 3)
(add-to-list 'default-frame-alist '(font . "Monaco-16"))
(add-to-list 'custom-theme-load-path "~/blizzard/src/blizzard-colors/emacs")
(load-theme 'blizzard 't)

;; Diminish
;; (require 'diminish)
;; (diminish 'abbrev-mode)
;; (diminish 'company-mode)
;; (diminish 'global-whitespace-mode)
;; (diminish 'yas-minor-mode)

;; Start up the server
(server-start)

(provide 'init)
;;; init.el ends here
