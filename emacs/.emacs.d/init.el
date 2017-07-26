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

;; Higher GC threshold
(let ((gc-cons-threshold most-positive-fixnum)
       (file-name-handler-alit nil))

  ;; Package config
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; (setq use-package-verbose t)
  (eval-when-compile
    (require 'use-package))
  (require 'diminish)
  (require 'bind-key)

  ;; Custom File
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

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
    :defer 1
    :config
    (linum-relative-global-mode))

  ;; UTF-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

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

  ;; Don't bother having to type yes ever
  (fset 'yes-or-no-p 'y-or-n-p)

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
  (setq
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
  (use-package uniquify
    :config
    (setq uniquify-buffer-name-style 'post-forward))

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
    ;whitespace-trailing 'whitespace-trailing
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
    :defer 1
    :config
    (nvm-use "6.10.3"))

  ;; EditorConfig
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

  ;; Recentf
  (use-package recentf
    :ensure t
    :commands recentf-open-files
    :functions tags-completion-table
    :bind ("C-x C-r" . recentf-ido-find-file)
    ("C-M-." . ido-find-tag)
    :config
    (setq recentf-arrange-by-rule-subfilter 'recentf-sort-directories-ascending
      recentf-arrange-rules '(("Elisp files (%d)" ".\\.el\\'")
                               ("Java files (%d)" ".\\.java\\'")
                               ("C/C++ files (%d)" "c\\(pp\\)?\\'")
                               ("SQL Files (%d)" ".\\.sql\\'")
                               ("Tcl Files (%d)" ".\\.tcl\\'")
                               ("Adp Files (%d)" ".\\.adp\\'")
                               ("Css Files (%d)" ".\\.css\\'")
                               ("Ruby Files (%d)" ".\\.rb\\'")
                               ("Javascript Files (%d)" ".\\.jsx?\\'")
                               ("Jade/Pug Files (%d)" ".\\.\\(jade\\)\\|\\(pug\\)\\'")
                               ("Erb Files (%d)" ".\\.erb\\'"))
      recentf-max-menu-items 100
      recentf-max-saved-items 100
      recentf-menu-filter 'recentf-arrange-by-rule
      recentf-show-file-shortcuts-flag nil)
    (defun bookmark-ido-find-file ()
      "Find a bookmark using Ido."
      (interactive)
      (let ((bm (ido-completing-read "Choose bookmark: "
                  (bookmark-all-names)
                  nil t)))
        (when bm
          (bookmark-jump bm))))
    (defun ido-find-tag ()
      "Find a tag using ido."
      (interactive)
      (tags-completion-table)
      (let (tag-names)
        (mapc (lambda (x)
                (unless (integerp x)
                  (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
        (xref-find-definitions (ido-completing-read "Tag: " tag-names))))
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
    (recentf-mode 1))

  ;; Bookmarks
  (use-package bookmarks
    :commands bookmark-all-names)

  ;; Ido
  (use-package ido
    :ensure t
    :bind
    ("M-i" . ido-goto-symbol)
    ("C-x r b" . bookmark-ido-find-file)
    :config
    (setq ido-confirm-unique-completion t
      ido-enable-flex-matching t
      ido-everywhere t
      ido-max-prospects 6
      ido-use-faces nil
      ido-use-virtual-buffers t
      ido-show-dot-for-dired t)
    (defun bookmark-ido-find-file ()
      "Find a bookmark using Ido."
      (interactive)
      (let ((bm (ido-completing-read "Choose bookmark: "
                  (bookmark-all-names)
                  nil t)))
        (when bm
          (bookmark-jump bm))))
    (ido-mode 1))

  (use-package flx-ido
    :ensure t
    :defer 1
    :config
    (flx-ido-mode 1))

  (use-package avy
    :ensure t
    :bind ("C-c j" . avy-goto-word-or-subword-1))

  (use-package prettier-js
    :ensure t
    :bind ("<f8>" . prettier-js))

  ;; Fancy Kill Ring
  (use-package browse-kill-ring
    :ensure t
    :defer 1
    :config
    (browse-kill-ring-default-keybindings))

  ;; Smex
  (use-package smex
    :ensure t
    :bind
    ("M-x" . smex)
    ("M-X" . smex-major-mode-commands)
    ("C-c C-c M-x" . execute-extended-command)
    :config
    (smex-initialize))

  ;; Projectile
  (use-package projectile
    :ensure t
    :bind (("C-x f" . projectile-find-file))
    :config
    (projectile-mode))

  ;; Haskell
  (use-package haskell-mode
    :ensure t
    :mode "\\.hs\\'"
    :config
    (setq haskell-font-lock-symbols t))

  ;; Markdown
  (use-package markdown-mode
    :ensure t
    :mode "\\.md\\'"
    :config
    (add-hook 'markdown-mode-hook
      (lambda ()
        (fset 'markdown-links
          (lambda
            (&optional arg)
            "Keyboard macro."
            (interactive "p")
            (kmacro-exec-ring-item (quote ([19 60 97 13 19 104 114 101 102 61 34 13 67108896 19 34 13 left 23 18 60 97 13 91 93 40 25 41 67108896 19 62 13 23 67108896 19 60 13 left 23 18 91 93 13 right 25 19 60 47 97 62 13 backspace backspace backspace backspace] 0 "%d"))
              arg)))
        (local-set-key (kbd "C-c C-l") 'markdown-links))))

  ;; Ruby Files
  (use-package ruby-mode
    :ensure t
    :mode "\\.csv\\.csvbuilder\\'" "\\.json\\.jbuilder\\'" "\\.ru\\'" "\\.gemspec\\'" "\\Gemfile\\'" "\\Guardfile\\'" "\\.builder\\'" "\\.god\\'" "Rakefile\\'")

  ;; Cucumber
  (use-package feature-mode
    :ensure t
    :mode "\\.feature\\'"
    :config
    (setq feature-default-language "en"))

  ;; Magit - Required for modeline
  (use-package magit
    :ensure t
    :bind ("C-c i" . magit-status))

  ;; Web Mode
  (use-package web-mode
    :ensure t
    :mode "\\.erb\\'" "\\.html?\\'" "\\.ftl\\'")

  ;; Javascript
  (use-package json-mode
    :ensure t
    :mode "\\.json\\'"
    :config
    (add-hook 'json-mode-hook
      (lambda ()
        (setq js-indent-level 2))))

  (use-package js2-mode
    :ensure t
    :mode "\\.js\\'" "\\.js\\.erb\\'"
    :config
    (add-hook 'js2-mode-hook
      (lambda ()
        (setq js-indent-level 2)
        (setq indent-tabs-mode nil)
        (setq tab-width 2)
        (setq js2-mode-show-parse-errors nil)
        (setq js2-mode-show-strict-warnings nil)))
    ;; Js2-refactor
    (use-package js2-refactor
      :ensure t
      :config
      (add-hook 'js2-mode-hook 'js2-refactor-mode)))


  (use-package flycheck
    :ensure t
    :defer 1
    :config
    (global-flycheck-mode)
    (setq-default flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)))

    (defun kelsin/use-eslint-from-node-modules ()
      "Find eslint binary in node_modules folder if possible."
      (let* ((root (locate-dominating-file
                     (or (buffer-file-name) default-directory)
                     "node_modules"))
              (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                          root))))
        (when (and eslint (file-executable-p eslint))
          (setq-local flycheck-javascript-eslint-executable eslint))))
    (add-hook 'flycheck-mode-hook #'kelsin/use-eslint-from-node-modules))

  ;; Rainbow
  (use-package rainbow-mode
    :ensure t
    :commands rainbow-mode
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'sass-mode-hook 'rainbow-mode)
    (add-hook 'scss-mode-hook 'rainbow-mode)
    (add-hook 'less-css-mode-hook 'rainbow-mode))

  ;; Scss
  (use-package scss-mode
    :ensure t
    :mode "\\.scss\\'"
    :config
    (setq scss-compile-at-save nil))

  ;; nxml formatting
  (use-package nxml
    :ensure t
    :mode ("\\.adp\\'" . nxml-mode)
    :defines nxml-sexp-element-flag nxml-slash-auto-complete-flag
    :config
    (add-hook 'nxml-mode-hook
      (lambda ()
        (setq
          nxml-sexp-element-flag t
          nxml-slash-auto-complete-flag t
          indent-tabs-mode 't
          tab-width 2))))

  ;; vue
  (use-package vue-mode
    :ensure t
    :mode "\\.vue\\'")

  ;; jsx
  (use-package rjsx-mode
    :ensure t
    :bind (:map evil-insert-state-map
            ("C-d" . rjsx-delete-creates-full-tag))
    :mode "\\.jsx\\'" "components\\/.*\\.js\\'")

  ;; Byte compile the current file automatically
  (defun byte-compile-current-buffer ()
    "'byte-compile' current buffer if it's 'emacs-lisp-mode' and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
            (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'byte-compile-current-buffer)

  ;; Snippets
  (use-package yasnippet
    :defer 2
    :ensure t
    :diminish yas-minor-mode
    :config
    (progn
      (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
      (yas-global-mode 1)
      (add-hook 'term-mode-hook
        (lambda()
          (setq yas-dont-activate-functions t)))))

  ;; Reindent then newline and indent
  (global-set-key (kbd "M-RET") 'reindent-then-newline-and-indent)

  ;; Bind fixup-whitespace
  (global-set-key "\C-xw" 'fixup-whitespace)

  ;; Function keybindings
  (global-set-key [f1] 'info)
  (global-set-key [f2] 'kelsin/google)

  ;; No suspend in terminal
  (global-unset-key (kbd "C-z"))

  ;; Modeline
  (use-package smart-mode-line
    :ensure t
    :init
    (setq sml/theme 'dark)
    :config
    (sml/setup))

  ;; Load Theme and Font
  (setq-default line-spacing 3)
  (add-to-list 'default-frame-alist '(font . "Monaco-16"))
  (add-to-list 'custom-theme-load-path "~/blizzard/src/blizzard-colors/emacs")
  (load-theme 'blizzard 't)

  ;; Start up the server
  (server-start)

  ;; Evil
  (use-package evil
    :ensure t
    :demand t
    :bind ( :map evil-motion-state-map
            ("L" . evil-forward-arg)
            ("H" . evil-backward-arg)
            :map evil-normal-state-map
            ("L" . evil-forward-arg)
            ("H" . evil-backward-arg)
            ("K" . evil-jump-out-args)
            :map evil-inner-text-objects-map
            ("i" . evil-inner-arg)
            :map evil-outer-text-objects-map
            ("a" . evil-outer-arg))
    :init
    ;; Cursors
    (setq evil-emacs-state-cursor '("blue" hbar))
    (setq evil-normal-state-cursor '("green" hbar))
    (setq evil-visual-state-cursor '("orange" hbar))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))

    :config
    (evil-mode 1)

    ;; Modes to use emacs mode in
    (add-to-list 'evil-emacs-state-modes 'nav-mode)
    (add-to-list 'evil-emacs-state-modes 'magit-mode)
    (add-to-list 'evil-emacs-state-modes 'dired-mode)

    ;; Undo Tree Mode
    (use-package undo-tree
      :ensure t
      :diminish
      :config
      (global-undo-tree-mode))

    ;; Evil Matchit
    (use-package evil-matchit
      :ensure t
      :commands evilmi-jump-items
      :config
      (global-evil-matchit-mode))

    ;; Evil Commentary
    (use-package evil-commentary
      :ensure t
      :defer 1
      :diminish
      :config
      (evil-commentary-mode 1))

    ;; Evil Surround
    (use-package evil-surround
      :ensure t
      :defer 1
      :config
      (global-evil-surround-mode 1))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
