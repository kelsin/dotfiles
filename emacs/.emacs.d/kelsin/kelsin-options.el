;;; kelsin-options.el --- My own custom (simple) emacs settings

;; Copyright (C) 2007 Christopher Giroir

;; Author: Christopher Giroir <kelsin@valefor.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Just my own personal (small) settings

;;; Code:

;; Turn off file variables
;; See: http://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html#Safe-File-Variables
(setq enable-local-variables nil
      enable-local-eval nil)

;; Don't warn me about loading large files (TAGS files)
(setq large-file-warning-threshold nil)

;; Never us dialog windows
(setq use-dialog-box nil)

;; Set Shell to bash
(setq shell-file-name "/bin/bash")

;; Linum Mode
(global-linum-mode)
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-global-mode))

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-sort-flag 1)

;; Case Fold Search
(setq case-fold-search t)
(setq tags-case-fold-search t)

;; Dired Searches only use filename
(setq dired-isearch-filenames t)
(setq dired-use-ls-dired nil)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Expiration Date of Buffers
(setq clean-buffer-list-delay-general 1)

;; Higher GC threshold
(setq gc-cons-threshold 20000000)

;; Saving place in buffers
(save-place-mode 1)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Clean up startup and splash screen
(setq
  inhibit-splash-screen t
  inhibit-startup-echo-area-message t
  inhibit-startup-message t
  initial-scratch-message nil)

;; No backup files
(setq make-backup-files nil)

;; Set paren style
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Set preferred code/tab style
(setq-default c-default-style "java"
              c-basic-offset 2
              css-indent-offset 2
              mail-indentation-spaces 4
              ruby-indent-level 2
              sh-basic-offset 2
              sh-indentation 2
              css-indent-offset 2
              tab-width 2
              indent-tabs-mode nil)

;; Turn on column and line numbers in the mode line
(setq column-number-mode t)
(setq line-number-mode t)

;; Truncate instead of wrapping lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows 't)

;; Highlight where the marked region is
(setq transient-mark-mode t)

;; Fill test to 80 columns
(set-default 'fill-column 80)

;; Auto load compressed files correctly
(auto-compression-mode 1)

;; No annoying beep or flashes
(setq ring-bell-function 'ignore)

;; Gui Options
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Tramp
(setq tramp-default-method "ssh"
      tramp-syntax 'ftp
      tramp-verbose 8)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; User
(setq user-full-name "Christopher Giroir"
      user-mail-address "kelsin@valefor.com")

;; Yellow bar cursor
(add-to-list 'default-frame-alist '(cursor-type . bar))
(add-to-list 'initial-frame-alist '(cursor-type . bar))
(set-cursor-color "yellow")

;; Ansi colors in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; Default to unified diffs
(setq diff-switches "-u")

;; Always end a file with a newline
(setq require-final-newline 't)

;; Make files with #! at the beginning executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Visible Bell
(setq visible-bell t)

;; WDired
(setq wdired-allow-to-change-permissions 'advanced)

;; Whitespace display options
(setq whitespace-display-mappings '((space-mark 32 [183] [46])    ; · or .
                                    (newline-mark 10 [172 10])    ; ¬
                                    (tab-mark 9 [8594 9] [92 9])) ; → or \
      whitespace-line-column nil
      whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab tab-mark newline-mark)
      whitespace-trailing 'whitespace-trailing
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
      require-final-newline t)
(global-whitespace-mode 1)

;; Windmove
(windmove-default-keybindings)

;; Enable special commands
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Disable VC
(setq vc-handled-backends nil)

;; Global HL Mode Line
(global-hl-line-mode)

;; Turn off redefadvice warnings
;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; Windows Settings
(if (eq system-type 'windows-nt)
    (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

;; Mac Settings
(if (and
      (eq system-type 'darwin)
      (featurep 'ns))
  (progn
    (setq ns-alternate-modifier 'super
      ns-command-modifier 'meta
      ns-extended-platform-support-mode t
      ns-pop-up-frames nil
      ns-use-qd-smoothing nil)
    (add-hook 'server-switch-hook
      (lambda ()
        (when (current-local-map)
          (use-local-map (copy-keymap (current-local-map))))
        (recenter)
        (local-set-key (kbd "C-x k") 'server-edit)
        (local-set-key (kbd "C-c C-c") 'server-edit)
        (local-set-key (kbd "C-c c") 'server-edit)))))

;; Multiple Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; Projectile switch project action
(setq projectile-switch-project-action 'projectile-dired)

;; Don't auto save
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Dired human readable
(setq dired-listing-switches "-alh")

;; Backup files into temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; NVM
(use-package nvm
  :ensure t
  :config
  (nvm-use "6.10.3"))

(provide 'kelsin-options)
;;; kelsin-options.el ends here
