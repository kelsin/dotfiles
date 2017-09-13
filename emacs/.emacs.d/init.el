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

  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; Setup exec-path
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setenv "NODENV_VERSION" "8.2.1")
    (setenv "RBENV_VERSION" "2.4.1")
    :config
    (exec-path-from-shell-initialize))

  ;; Turn off file variables
  ;; See: http://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html#Safe-File-Variables
  (setq enable-local-variables nil
    enable-local-eval nil)

  ;; Don't warn me about loading large files (TAGS files)
  (setq large-file-warning-threshold nil)

  ;; Never use dialog windows
  (setq use-dialog-box nil)

  ;; Set Shell to bash
  (setq shell-file-name "/bin/bash")

  ;; Org Mode
  (use-package org
    :ensure t
    :init
    (setq org-directory "~/org")
    (setq org-default-notes-file "~/org/todo.org")
    (setq org-log-done t)
    (setq org-startup-folded nil)
    (setq org-src-preserve-indentation t)
    :defines org-capture-templates
    :mode ("\\.org\\'" . org-mode)
    :bind
    ("C-c c" . org-capture)
    ("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    :config
    (use-package ob-restclient
      :ensure t)

    (setq org-capture-templates
      '( ("c" "Code" entry (file+datetree "~/org/code.org") "* %?\n\n  %a")
         ("s" "Song" entry (file+datetree "~/org/songs.org") "* %U\n  %?")
         ("t" "Todo" entry (file+headline "~/org/todo.org" "Quick") "* TODO %?\n  %i")))

    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

    (org-babel-do-load-languages
      'org-babel-load-languages
      '((dot . t)
         (plantuml . t)
         (restclient . t)))

    (setq org-latex-listings t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.14/libexec/plantuml.jar")

    (use-package ox-reveal
      :ensure t)

    (use-package org-bullets
      :ensure t
      :commands org-bullets-mode
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

  ;; Dired Details+
  (use-package dired-details+
    :ensure t)

  ;; Dired+
  (use-package dired+
    :disabled t
    :ensure t)

  ;; Prettify Symbols
  (global-prettify-symbols-mode 1)
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  ;; Protobuf mode
  (use-package protobuf-mode
    :ensure t
    :mode "\\.proto\\'")

  ;; Plantuml mode
  (use-package plantuml-mode
    :ensure t
    :mode "\\.uml\\'")

  ;; Ledger Mode
  (use-package ledger-mode
    :ensure t
    :mode "\\.ledger\\'"
    :config
    (setq ledger-clear-whole-transactions 't)

    (use-package flycheck-ledger
      :ensure t))

  ;; Linum Mode
  (use-package linum
    :ensure t
    :defer 1
    :diminish linum-mode
    :config
    (global-linum-mode))
  (use-package linum-relative
    :ensure t
    :defer 1
    :diminish linum-relative-mode
    :config
    (linum-relative-global-mode))

  ;; UTF-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

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
  (use-package saveplace
    :config
    (save-place-mode 1))

  ;; Clean up startup and splash screen
  (setq
    inhibit-splash-screen t
    inhibit-startup-echo-area-message t
    inhibit-startup-message t
    initial-scratch-message nil)

  ;; No backup files
  (setq make-backup-files nil)

  ;; Gnus
  (use-package gnus
    :config
    (setq gnus-select-method '(nntp "news.gwene.org")))

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

  ;; Restclient
  (use-package restclient
    :ensure t
    :mode ("\\.rest\\'" . restclient-mode)
    :config
    (use-package company-restclient
      :ensure t
      :config
      (add-to-list 'company-backends 'company-restclient)))

  ;; Ace Window
  (use-package ace-window
    :bind ("C-c w" . ace-window)
    :ensure t)

  ;; Ivy
  (use-package counsel
    :ensure t
    :demand t
    :diminish ivy-mode
    :bind
    ("M-x" . counsel-M-x)
    ("C-s" . swiper)
    ("C-x C-f" . counsel-find-file)
    ("C-x C-r" . counsel-recentf)
    ("C-x r b" . counsel-bookmark)
    ("C-c C-a" . counsel-apropos)
    ("C-c C-g" . counsel-ag)
    :init
    (use-package smex
      :ensure t)
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq enable-recursive-minibuffers t)
    (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
         (t . ivy--regex-fuzzy)))
    (setq ivy-initial-inputs-alist nil)
    (ivy-mode 1)

    (use-package counsel-projectile
      :ensure t
      :config
      (counsel-projectile-on))

    (use-package counsel-gtags
      :ensure t)

    (use-package counsel-spotify
      :ensure t)

    (use-package flx
      :ensure t))

  ;; Dash
  (use-package dash-at-point
    :ensure t
    :bind
    ("<f1>" . dash-at-point)
    ("C-c C-d" . dash-at-point))

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
  (setq wdired-allow-to-change-permissions 't)

  ;; HTMLIZE
  (use-package htmlize
    :ensure t
    :config
    (setq htmlize-head-tags "    <style>body { font-size: 28px; }</style>
"))

  ;; Whitespace display options
  (use-package whitespace
    :diminish global-whitespace-mode global-whitespace-newline-mode
    :demand t
    :bind ("C-x w" . fixup-whitespace)
    :config
    (setq whitespace-display-mappings '((space-mark 32 [183] [46])    ; · or .
                                         (newline-mark 10 [172 10])    ; ¬
                                         (tab-mark 9 [8594 9] [92 9])) ; → or \
      whitespace-line-column nil
      whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab tab-mark newline-mark)
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
      require-final-newline t)
    (global-whitespace-mode 1))

  ;; Windmove
  (use-package windmove
    :config
    (windmove-default-keybindings))

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
  (use-package hl-line
    :disabled t
    :config
    (global-hl-line-mode))

  ;; Turn off redefadvice warnings
  ;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
  (setq ad-redefinition-action 'accept)

  ;; Mac Settings
  (if (and
        (eq system-type 'darwin)
        (featurep 'ns))
    (setq ns-alternate-modifier 'super
      ns-command-modifier 'meta
      ns-extended-platform-support-mode t
      ns-pop-up-frames nil
      ns-use-qd-smoothing nil))

  ;; Multiple Cursors
  (use-package multiple-cursors
    :disabled t
    :ensure t
    :bind (("C-S-c C-S-c" . mc/edit-lines)
            ("C->" . mc/mark-next-like-this)
            ("C-<" . mc/mark-previous-like-this)
            ("C-c C->" . mc/mark-all-like-this)))

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

  ;; All The Icons
  (use-package all-the-icons
    :ensure t
    :config
    (use-package all-the-icons-dired
      :ensure t
      :diminish all-the-icons-dired-mode
      :config
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

  (use-package neotree
    :load-path "~/src/emacs-neotree"
    :bind ("<f9>" . neotree-toggle)
    :config
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
      neo-smart-open t
      neo-window-width 30
      neo-auto-indent-point t
      neo-autorefresh t
      neo-force-change-root t))

  ;; EditorConfig
  (use-package editorconfig
    :ensure t
    :diminish editorconfig-mode
    :config
    (editorconfig-mode 1))

  ;; Tagging
  (use-package etags)

  ;; Bookmarks
  (use-package bookmark
    :config
    (setq bookmark-save-flag 1)
    (setq bookmark-sort-flag 1))

  ;; Recentf
  (use-package recentf
    :ensure t
    :commands recentf-open-files
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
    (recentf-mode 1))

  ;; Ido
  (use-package ido
    :disabled t
    :ensure t
    :bind
    ("M-i" . ido-goto-symbol)
    ("C-x r b" . bookmark-ido-find-file)
    :functions bookmark-all-names
    :config
    (setq ido-confirm-unique-completion t
      ido-enable-flex-matching t
      ido-everywhere t
      ido-max-prospects 6
      ido-use-faces nil
      ido-use-virtual-buffers t
      ido-show-dot-for-dired t)
    (ido-mode 1))

  (use-package flx-ido
    :disabled t
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

  ;; Graphviz
  (use-package graphviz-dot-mode
    :ensure t
    :mode "\\.dot\\'")

  ;; Vimrc mode
  (use-package vimrc-mode
    :ensure t
    :mode "\\.vim\\(rc\\)?\\'")

  ;; Persp-mode
  (use-package persp-mode
    :ensure t
    :init
    (setq persp-keymap-prefix (kbd "C-c C-p"))
    :config
    (persp-mode)
    (with-eval-after-load "persp-mode"
      (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

      ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)

      (persp-def-auto-persp "projectile"
        :parameters '((dont-save-to-file . t)
                       (persp-mode-projectile-bridge . t))
        :hooks '(projectile-before-switch-project-hook
                  projectile-after-switch-project-hook
                  projectile-find-file-hook
                  find-file-hook)
        :dyn-env '((after-switch-to-buffer-adv-suspend t))
        :switch 'frame
        :predicate
        #'(lambda (buffer &optional state)
            (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
              (and
                projectile-mode
                (buffer-live-p buffer)
                (buffer-file-name buffer)
                ;; (not git-commit-mode)
                (projectile-project-p)
                (or state t))))
        :get-name
        #'(lambda (state)
            (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
              (push (cons 'persp-name
                      (concat "p) "
                        (with-current-buffer (alist-get 'buffer state)
                          (projectile-project-name))))
                state)
              state))
        :on-match
        #'(lambda (state)
            (let ((hook (alist-get 'hook state))
                   (persp (alist-get 'persp state))
                   (buffer (alist-get 'buffer state)))
              (case hook
                (projectile-before-switch-project-hook
                  (let ((win (if (minibuffer-window-active-p (selected-window))
                               (minibuffer-selected-window)
                               (selected-window))))
                    (when (window-live-p win)
                      (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                        (window-buffer win)))))

                (projectile-after-switch-project-hook
                  (when (buffer-live-p
                          persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                    (let ((win (selected-window)))
                      (unless (eq (window-buffer win)
                                persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                        (set-window-buffer
                          win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

                (find-file-hook
                  (setcdr (assq :switch state) nil)))
              (if (case hook
                    (projectile-before-switch-project-hook nil)
                    (t t))
                (persp--auto-persp-default-on-match state)
                (setcdr (assq :after-match state) nil)))
            state)
        :after-match
        #'(lambda (state)
            (when (eq 'find-file-hook (alist-get 'hook state))
              (run-at-time 0.5 nil
                #'(lambda (buf persp)
                    (when (and (eq persp (get-current-persp))
                            (not (eq buf (window-buffer (selected-window)))))
                      ;; (switch-to-buffer buf)
                      (persp-add-buffer buf persp t nil)))
                (alist-get 'buffer state)
                (get-current-persp)))
            (persp--auto-persp-default-after-match state)))

      (with-eval-after-load "ivy"
        (add-hook 'ivy-ignore-buffers
          #'(lambda (b)
              (when persp-mode
                (let ((persp (get-current-persp)))
                  (if persp
                    (not (persp-contain-buffer-p b persp))
                    nil)))))

        (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
            '((persp-kill-buffer   . nil)
               (persp-remove-buffer . nil)
               (persp-add-buffer    . nil)
               (persp-switch        . nil)
               (persp-window-switch . nil)
               (persp-frame-switch  . nil)))))))

  ;; Projectile
  (use-package projectile
    :ensure t
    :demand t
    :bind
    (:map projectile-mode-map
      ("C-c p g" . counsel-projectile-rg))
    :config
    (projectile-mode)
    (setq projectile-switch-project-action #'projectile-dired))

  ;; Terraform
  (use-package terraform-mode
    :ensure t
    :mode "\\.tf\\'"
    :config
    (use-package company-terraform
      :ensure t))

  ;; Pug
  (use-package pug-mode
    :ensure t
    :mode "\\.pug\\'" "\\.jade\\'")

  ;; Yaml
  (use-package yaml-mode
    :ensure t
    :mode "\\.ya?ml\\'")

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

  ;; Magit
  (use-package magit
    :ensure t
    :bind ("C-c i" . magit-status))

  ;; Ember Mode
  (use-package ember-mode
    :ensure t
    :commands ember-mode)

  ;; Web Mode
  (use-package web-mode
    :ensure t
    :mode "\\.erb\\'" "\\.html?\\'" "\\.ftl\\'")

  (use-package elm-mode
    :ensure t
    :mode "\\.elm\\'")

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
        (setq js2-mode-show-strict-warnings nil)
        (push '("*" . ?×) prettify-symbols-alist)
        (push '("/" . ?∕) prettify-symbols-alist)
        (push '("%" . ?÷) prettify-symbols-alist)
        (push '("&&" . ?∧) prettify-symbols-alist)
        (push '("||" . ?∨) prettify-symbols-alist)
        (push '("!" . ?¬) prettify-symbols-alist)
        (push '("==" . ?＝) prettify-symbols-alist)
        (push '("===" . ?≡) prettify-symbols-alist)
        (push '("!=" . ?≠) prettify-symbols-alist)
        (push '("!==" . ?≢) prettify-symbols-alist)
        (push '("null" . ?∅) prettify-symbols-alist)
        (push '("function" . ?λ) prettify-symbols-alist)
        (push '("return" . ?⇐) prettify-symbols-alist)
        (push '("->" . ?→) prettify-symbols-alist)))

    ;; Js2-refactor
    (use-package js2-refactor
      :ensure t
      :diminish js2-refactor-mode
      :config
      (add-hook 'js2-mode-hook 'js2-refactor-mode)))

  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
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
    :diminish rainbow-mode
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

  ;; No suspend in terminal
  (global-unset-key (kbd "C-z"))

  ;; Smartparens
  (use-package smartparens-config
    :ensure smartparens
    :config
    (show-smartparens-global-mode t)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

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
  (use-package server
    :defer 1
    :config
    (unless (server-running-p) (server-start))
    (add-hook 'server-switch-hook
      (lambda ()
        (when (current-local-map)
          (use-local-map (copy-keymap (current-local-map))))
        (recenter)
        (local-set-key (kbd "C-x k") 'server-edit)
        (local-set-key (kbd "C-c C-c") 'server-edit)
        (local-set-key (kbd "C-c c") 'server-edit))))

  (use-package company
    :ensure t
    :defer 1
    :diminish company-mode
    :config
    (global-company-mode))

  ;; Evil
  (use-package evil
    :ensure t
    :demand t
    :bind ( :map evil-motion-state-map
            ("L" . evil-forward-arg)
            ("H" . evil-backward-arg)
            :map evil-insert-state-map
            ("C-e" . end-of-line)
            :map evil-normal-state-map
            ("L" . evil-forward-arg)
            ("H" . evil-backward-arg)
            ("K" . evil-jump-out-args)
            ("C-e" . evil-end-of-line)
            :map evil-motion-state-map
            ("C-e" . evil-end-of-line)
            :map evil-visual-state-map
            ("C-e" . evil-end-of-line)
            :map evil-inner-text-objects-map
            ("i" . evil-inner-arg)
            :map evil-outer-text-objects-map
            ("a" . evil-outer-arg))
    :init

    ;; Cursors
    (setq evil-emacs-state-cursor '("#007dbf" hbar))
    (setq evil-normal-state-cursor '("#8cda38" hbar))
    (setq evil-visual-state-cursor '("#ea7b00" hbar))
    (setq evil-insert-state-cursor '("#ff2e2e" bar))
    (setq evil-replace-state-cursor '("#ff2e2e" bar))
    (setq evil-operator-state-cursor '("#00aeef" hollow))

    (setq-default evil-cross-lines t)
    (setq-default evil-find-skip-newlines t)
    (setq-default evil-move-beyond-eol t)

    :config
    (evil-mode 1)

    ;; Modes to use emacs mode in
    (add-to-list 'evil-emacs-state-modes 'nav-mode)
    (add-to-list 'evil-emacs-state-modes 'magit-mode)
    (add-to-list 'evil-emacs-state-modes 'dired-mode)
    (add-to-list 'evil-emacs-state-modes 'neotree-mode)

    ;; Evil Leader
    (use-package evil-leader
      :ensure t
      :config
      (global-evil-leader-mode))

    ;; Undo Tree Mode
    (use-package undo-tree
      :ensure t
      :diminish undo-tree-mode undo-tree-visualizer-selection-mode
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
      :diminish evil-commentary-mode
      :config
      (evil-commentary-mode 1))

    ;; Evil Surround
    (use-package evil-surround
      :ensure t
      :defer 1
      :config
      (global-evil-surround-mode 1))

    (use-package evil-mc
      :ensure t
      :defer 1
      :diminish evil-mc-mode
      :config
      (global-evil-mc-mode 1))

    (use-package evil-org
      :ensure t
      :diminish evil-org-mode
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'evil-org-mode-hook
        (lambda ()
          (evil-org-set-key-theme))))))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((agenda)
                           (projects . 10)
                           (recents . 5)))
  (dashboard-setup-startup-hook))

(provide 'init)
;;; init.el ends here
