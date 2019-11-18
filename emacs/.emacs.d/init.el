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
         (file-name-handler-alist nil))

    ;; Package config
    (setq package-enable-at-startup nil)
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (package-initialize 'noactivate)

    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

    (package-initialize)

    (setq use-package-verbose t)

    (eval-when-compile
        (require 'use-package))
    (require 'bind-key)

    ;; Custom File
    (setq custom-file "~/.emacs.d/custom.el")
    (load custom-file)

    (setenv "LC_ALL" "en_US.UTF-8")
    (setenv "LANG" "en_US.UTF-8")

    ;; UTF-8
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

    ;; Case Fold Search
    (setq case-fold-search t)
    (setq tags-case-fold-search t)

    ;; Enable special commands
    (put 'set-goal-column 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'narrow-to-page 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)
    (put 'dired-find-alternate-file 'disabled nil)

    ;; Line numbers
    (setq-default display-line-numbers-type 'relative
        display-line-numbers-current-absolute t
        display-line-numbers-width 4
        display-line-numbers-widen t)

    ;; Ansi colors for compilation output
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
        (ansi-color-apply-on-region compilation-filter-start (point)))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

    ;; Don't warn me about loading large files (TAGS files)
    (setq large-file-warning-threshold nil)

    ;; Never use dialog windows
    (setq use-dialog-box nil)

    ;; Set Shell to bash
    (setq shell-file-name "/bin/bash")

    ;; Set exec-path
    (setenv "PATH" (concat "/usr/local/share/dotnet:/usr/local/bin:" (getenv "PATH")))
    (setq exec-path (append '("/Users/cgiroir/.nodenv/shims/" "/Users/cgiroir/.rbenv/shims/" "/usr/local/bin") exec-path))

    ;; Add a lisp folder and my functions package to the load path
    (add-to-list 'load-path "~/.emacs.d/lisp/")
    (add-to-list 'load-path "~/.emacs.d/kelsin/")

    ;; Don't prompt for compile commands and auto jump to first error
    (setq compilation-read-command nil)
    (setq compilation-auto-jump-to-first-error 't)
    (setq compilation-scroll-output 'first-error)
    (setq compilation-window-height 10)

    ;; Load my functions
    (use-package kelsin-functions
        :bind (("<f8>" . reformat-buffer))
        :config
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "f" '(reformat-buffer :which-key "reformat-buffer")))

    ;; Load Theme and Font
    (setq-default line-spacing 3)
    (add-to-list 'default-frame-alist '(font . "Monaco-16"))
    (add-to-list 'custom-theme-load-path "~/src/blizzard-colors/emacs")
    (load-theme 'blizzard 't)

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

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; Expiration Date of Buffers
    (setq clean-buffer-list-delay-general 1)

    ;; Diminish Mode
    (use-package diminish
      :ensure t)

    ;; Saving place in buffers
    (use-package saveplace
        :config
        (save-place-mode 1))

    ;; Needed by other packages to provide good fuzzy finding
    (use-package flx
        :ensure t)

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
    (setq
        c-basic-offset 4
        c-offsets-alist '((substatement-open . 0)
                             (brace-list-open . 0)
                             (member-init-cont . 0)
                             (arglist-intro . +)
                             (arglist-close . 0)
                             (inlambda . 0)
                             (case-label . +)
                             (statement-case-open . 0))
        css-indent-offset 4
        mail-indentation-spaces 4
        ruby-indent-level 4
        sh-basic-offset 4
        tab-width 4
        lisp-backquote-indentation nil
        indent-tabs-mode nil)
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

    ;; Evil
    (use-package evil
        :ensure t
        :demand t
        :bind ( :map evil-insert-state-map
                  ("C-a" . beginning-of-line)
                  ("C-e" . end-of-line)
                  :map evil-normal-state-map
                  ("C-e" . evil-end-of-line)
                  :map evil-motion-state-map
                  ("C-s" . evil-search-forward)
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
        (add-to-list 'evil-emacs-state-modes 'easy-jekyll-mode)
        (add-to-list 'evil-emacs-state-modes 'neotree-mode))

    ;; Evil Matchit
    (use-package evil-matchit
        :ensure t
        :after evil
        :config
        (global-evil-matchit-mode))

    ;; Evil Commentary
    (use-package evil-commentary
        :ensure t
        :after evil
        :diminish evil-commentary-mode
        :config
        (evil-commentary-mode 1))

    ;; Evil Surround
    (use-package evil-surround
        :ensure t
        :after evil
        :config
        (global-evil-surround-mode 1))

    ;; Evil Magit
    (use-package evil-magit
        :ensure t
        :after '(evil magit))

    ;; Which Key
    (use-package which-key
        :ensure t
        :diminish which-key-mode
        :config
        (which-key-mode))

    ;; General
    (use-package general
        :ensure t
        :config
        (general-evil-setup)

        (general-define-key
            :states '(normal visual insert)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "c" '(:ignore t :which-key "customize")
            "ca" '(customize-apropos :which-key "apropos")
            "cl" '(display-line-numbers-mode :which-key "line numbers")
            "ch" '(hl-line-mode :which-key "highlight current line")
            "g" '(:ignore t :which-key "go to")
            "ge" '(next-error :which-key "next error")))

    ;; Company
    (use-package company
        :ensure t
        :defer 1
        :diminish company-mode
        :config
        (global-company-mode))

    ;; LSP
    (use-package lsp-mode
        :ensure t
        :commands lsp
        :config
        (general-define-key
            :states '(normal visual insert)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "l" '(:ignore t :which-key "lsp")
            "ld" '(lsp-describe-thing-at-point :which-key "describe thing at point")
            "lr" '(lsp-restart-workspace :which-key "restart workspace")
            "ls" '(lsp-describe-session :which-key "describe session")
            "gc" '(lsp-find-declaration :which-key "declaration")
            "gd" '(lsp-find-definition :which-key "definition")
            "gi" '(lsp-goto-implementation :which-key "implementation")
            "gr" '(lsp-find-references :which-key "references")
            "gt" '(lsp-goto-type-definition :which-key "type definition")
            "ge" '(next-error :which-key "next error")))

    (use-package lsp-ui
        :after lsp-mode
        :ensure t
        :commands lsp-ui-mode)

    (use-package company-lsp
        :after (company lsp-mode)
        :ensure t
        :commands company-lsp)

    (use-package ccls
        :after lsp-mode
        :ensure t
        :hook ((c-mode c++-mode objc-mode) .
                  (lambda () (require 'ccls) (lsp))))

    ;; Org Mode
    (use-package org
        :ensure t
        :init
        (setq org-directory "~/org")
        (setq org-default-notes-file "~/org/todo.org")
        (setq org-log-done t)
        (setq org-startup-folded nil)
        (setq org-src-preserve-indentation t)
        (setq org-link-file-path-type 'relative)
        (general-define-key
            :states '(normal visual insert)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "o" '(:ignore t :which-key "Org")
            "oc" '(org-capture :which-key "capture")
            "oa" '(org-agenda :which-key "agenda")
            "ol" '(org-store-link :which-key "store link"))
        :defines org-capture-templates
        :mode ("\\.org\\'" . org-mode)
        :bind
        ("C-c c" . org-capture)
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        :config
        (use-package ob-restclient
            :ensure t)

        (setq org-log-refile t)
        (setq org-refile-targets '((nil :maxlevel . 9)
                                      (org-agenda-files :maxlevel . 9)))
        (setq org-agenda-files '("~/org" "~/blizzard/src/org"))
        (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
        (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

        (defvar kelsin/org-capture-file-blizzard-todo
            "~/blizzard/src/org/todo.org"
            "File to use when saving new Blizzard TODO items with Org Capture.")
        (defvar kelsin/org-capture-file-todo
            "~/org/todo.org"
            "File to use when saving new personal TODO items with Org Capture.")

        (setq org-capture-templates
            '(("c" "Code" entry (file+datetree "~/org/code.org") "* %?\n\n  %a")
                 ("s" "Song" entry (file+datetree "~/org/songs.org") "* %U\n  %?")
                 ("t" "Personal TODO" entry
                     (file+headline kelsin/org-capture-file-todo "Quick")
                     "* TODO %?\n  %i")
                 ("b" "Blizzard Items")
                 ("bc" "Code Link" entry
                     (file+headline kelsin/org-capture-file-blizzard-todo "Code")
                     "* TODO %?\n  %a")
                 ("bd" "TODO with deadline" entry
                     (file+headline kelsin/org-capture-file-blizzard-todo "New")
                     "* TODO %?\n DEADLINE: %^{Deadline}T\n %i")
                 ("bn" "NEXT" entry
                     (file+headline kelsin/org-capture-file-blizzard-todo "New")
                     "* NEXT %?\n  %i"
                     :prepend 't)
                 ("bt" "TODO" entry
                     (file+headline kelsin/org-capture-file-blizzard-todo "New")
                     "* TODO %?\n  %i")
                 ))

        (setq org-todo-keyword-faces
            '(("TODO" . blizzard-bryellow)
                 ("NEXT" . blizzard-yellow)
                 ("DONE" . blizzard-brgreen)
                 ("CANCELLED" . blizzard-white)
                 ("WAITING" . blizzard-brred)))

        (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                       ("WAITING" ("WAITING" . t))
                       (done ("WAITING"))
                       ("TODO" ("WAITING") ("CANCELLED"))
                       ("NEXT" ("WAITING") ("CANCELLED"))
                       ("DONE" ("WAITING") ("CANCELLED")))))

        (setq org-agenda-custom-commands
            '(("b" "Blizzard Agenda"
                  ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                                  (org-agenda-span 'day)
                                  (org-agenda-ndays 1)
                                  (org-agenda-start-on-weekday nil)
                                  (org-agenda-start-day "+0d")
                                  (org-agenda-todo-ignore-deadlines nil)))
                      (tags-todo "-CANCELLED-ARCHIVE/!NEXT"
                          ((org-agenda-overriding-header "Next Tasks:")))
                      (tags-todo "+DEADLINE=\"\"+SCHEDULED=\"\"/!"
                          ((org-agenda-overriding-header "Unscheduled Tasks:")))
                      (agenda "" ((org-agenda-overriding-header "This Week:")
                                     (org-agenda-span 'week)
                                     (org-agenda-ndays 5)
                                     (org-agenda-start-on-weekday 1)
                                     (org-agenda-start-day "+0d")
                                     (org-agenda-todo-ignore-deadlines nil)))
                      (agenda "" ((org-agenda-overriding-header "Next Week:")
                                     (org-agenda-span 'week)
                                     (org-agenda-ndays 5)
                                     (org-agenda-start-on-weekday 1)
                                     (org-agenda-start-day "+7d")
                                     (org-agenda-todo-ignore-deadlines nil)))
                      ))))

        (general-define-key
            :states '(normal visual insert)
            :keymaps 'org-mode-map
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "od" '(org-deadline :which-key "deadline")
            "oe" '(org-export-dispatch :which-key "export")
            "op" '(org-priority :which-key "priority")
            "os" '(org-schedule :which-key "schedule")
            "ot" '(org-todo :which-key "todo"))

        (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                 (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

        (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

        (org-babel-do-load-languages
            'org-babel-load-languages
            '((dot . t)
                 (shell . t)
                 (plantuml . t)
                 (restclient . t)))

        (setq org-latex-listings t)
        (setq org-confirm-babel-evaluate nil)
        (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.14/libexec/plantuml.jar"))

    ;; Ox-reveal
    (use-package ox-reveal
        :ensure t
        :after org)

    ;; Ox-confluence
    (use-package ox-confluence
        :after org)

    ;; Org Bullets
    (use-package org-bullets
        :ensure t
        :after org
        :hook (org-mode . org-bullets-mode))

    ;; Prettify Symbols
    (global-prettify-symbols-mode 1)
    (setq prettify-symbols-unprettify-at-point 'right-edge)

    ;; Nginx mode
    (use-package nginx-mode
        :ensure t
        :mode "\\.conf\\'")

    ;; Protobuf mode
    (use-package protobuf-mode
        :ensure t
        :mode "\\.proto\\'")

    (use-package modern-cpp-font-lock
        :ensure t
        :hook (c++-mode . modern-c++-font-lock-mode))

    (use-package clang-format
        :ensure t
        :config
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'c++-mode-map
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "f" '(clang-format-buffer :which-key "clang-format")))

    (use-package lsp-mode
        :ensure t
        :commands lsp)

    (use-package lsp-ui
        :ensure t)

    (use-package company-lsp
        :after company
        :ensure t
        :commands company-lsp)

    (use-package company-c-headers
        :after company
        :ensure t
        :config
        (push 'company-c-headers company-backends))

    ;; Lua mode
    (use-package lua-mode
        :ensure t
        :mode "\\.lua\\'")

    ;; Groovy mode
    (use-package groovy-mode
        :ensure t
        :mode "\\.groovy\\'")

    ;; Plantuml mode
    (use-package plantuml-mode
        :ensure t
        :mode "\\.uml\\'")

    ;; Ledger Mode
    (use-package ledger-mode
        :ensure t
        :mode "\\.ledger\\'"
        :config
        (setq ledger-clear-whole-transactions 't))

    (use-package flycheck-ledger
        :ensure t
        :after (flycheck ledger-mode))

    ;; Dired
    (use-package dired-x
        :config
        ;; Dired Searches only use filename
        (setq dired-isearch-filenames t)
        (setq dired-use-ls-dired nil)
        (setq dired-omit-files (concat dired-omit-files "\\|^\\.git$"))
        (add-to-list 'dired-omit-extensions ".meta")
        (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

    ;; Restclient
    (use-package restclient
        :ensure t
        :mode ("\\.rest\\'" . restclient-mode)
        :config
        (use-package company-restclient
            :ensure t
            :config
            (add-to-list 'company-backends 'company-restclient)))

    ;; Avy
    (use-package avy
        :ensure t
        :bind ("C-c j" . avy-goto-word-or-subword-1)
        :init
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "ga" '(avy-goto-word-or-subword-1 :which-key "avy goto")
            "gl" '(avy-goto-line :which-key "avy goto line")))

    ;; Ace Window
    (use-package ace-window
        :ensure t
        :bind ("C-c w" . ace-window)
        :init
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "w" '(ace-window :which-key "ace window")))

    ;; Projectile
    (use-package projectile
        :ensure t
        :demand t
        :bind
        (:map projectile-mode-map
            ("C-c p" . projectile-command-map)
            ("C-c p g" . counsel-projectile-rg))
        :config
        (projectile-mode)

        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "p" '(:ignore t :which-key "Projectile")
            "p SPC" '(counsel-projectile :which-key "find file or buffer")
            "pb" '(counsel-projectile-switch-to-buffer :which-key "find buffer")
            "pc" '(projectile-compile-project :which-key "compile project")
            "pd" '(projectile-dired :which-key "dired")
            "pf" '(counsel-projectile-find-file :which-key "find file")
            "pg" '(counsel-projectile-rg :which-key "ripgrep")
            "pk" '(projectile-kill-buffers :which-key "kill buffers")
            "pp" '(counsel-projectile-switch-project :which-key "switch project")
            "pr" '(projectile-replace :which-key "replace")
            "pR" '(projectile-run-project :which-key "run project")
            "ps" '(projectile-save-project-buffers :which-key "save project")
            "pt" '(projectile-test-project :which-key "test project")))

    ;; Projectile Rails
    (use-package projectile-rails
        :ensure t
        :after projectile)

    ;; Better fuzzy seraching in Ivy
    (use-package flx
        :ensure t)

    ;; Ivy
    (use-package ivy
        :diminish ivy-mode
        :ensure t
        :config
        (setq ivy-use-virtual-buffers t)
        (setq ivy-count-format "%d/%d ")
        (setq ivy-re-builders-alist
            '((swiper . ivy--regex-plus)
                 (t . ivy--regex-fuzzy)))
        (setq ivy-initial-inputs-alist nil)
        (ivy-mode 1))

    (use-package counsel
        :ensure t
        :after ivy
        :bind
        ("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-x C-r" . counsel-recentf)
        ("C-x r b" . counsel-bookmark)
        ("C-c C-a" . counsel-apropos)
        ("C-c C-g" . counsel-ag)
        :config
        (setq counsel-rg-base-command "rg -M 120 -S --no-heading --line-number --color never %s .")
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "s" '(swiper :which-key "swipe")))

    (use-package counsel-projectile
        :ensure t
        :after (counsel projectile)
        :hook (projectile-mode . counsel-projectile-mode)
        :config
        (counsel-projectile-modify-action
            'counsel-projectile-switch-project-action
            '((default "D"))))

    ;; Dash
    (use-package dash-at-point
        :ensure t
        :bind
        ("<f1>" . dash-at-point)
        ("C-c C-d" . dash-at-point)
        :init
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "d" '(dash-at-point :which-key "lookup in dash")))

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
        :ensure t
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

    ;; Fancy Kill Ring
    (use-package browse-kill-ring
        :ensure t
        :bind ( :map evil-insert-state-map
                  ("M-y" . browse-kill-ring)))

    ;; DumbJump
    (use-package dumb-jump
        :ensure t
        :config
        (setq dumb-jump-selector 'ivy)
        (setq dumb-jump-prefer-searcher 'rg)
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "j" '(dumb-jump-go :which-key "dumb-jump-go")
            "J" '(dumb-jump-back :which-key "dumb-jump-back")))

    ;; Graphviz
    (use-package graphviz-dot-mode
        :ensure t
        :mode "\\.dot\\'")

    ;; Vimrc mode
    (use-package vimrc-mode
        :ensure t
        :mode "\\.vim\\(rc\\)?\\'")

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
        :mode (("README\\.md\\'" . gfm-mode)
                  ("\\.md\\'" . markdown-mode)
                  ("\\.markdown\\'" . markdown-mode)))

    ;; Jekyll
    (use-package easy-jekyll
        :ensure t
        :init
        (setq easy-jekyll-basedir "~/src/mx.kelsin.net/")
        (setq easy-jekyll-url "https://mx.kelsin.net/")
        :config
        (general-define-key
            :states '(normal visual insert emacs)
            :keymaps 'override
            :prefix "SPC"
            :non-normal-prefix "C-SPC"
            "b" '(:ignore t :which-key "Jekyll")
            "bb" '(easy-jekyll :which-key "easy-jekyll")
            "bn" '(easy-jekyll-newpost :which-key "new post")))

    ;; Ruby Files
    (use-package ruby-mode
        :ensure t
        :mode "\\.csv\\.csvbuilder\\'" "\\.json\\.jbuilder\\'" "\\.ru\\'" "\\.gemspec\\'" "\\Gemfile\\'" "\\Guardfile\\'" "\\.builder\\'" "\\.god\\'" "Rakefile\\'")

    (use-package inf-ruby
        :ensure t)

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

    ;; C# Mode
    (use-package csharp-mode
        :ensure t
        :mode ("\\.cs\\'" . csharp-mode)
        :config
        (add-hook 'csharp-mode-hook
            (lambda ()
                (electric-pair-local-mode 1)))
        (use-package omnisharp
            :ensure t
            :config
            (general-define-key
                :states '(normal visual insert)
                :keymaps 'csharp-mode-map
                :prefix "SPC"
                :non-normal-prefix "C-SPC"
                "gd" '(omnisharp-go-to-definition :which-key "definition")
                "gr" '(omnisharp-find-usages :which-key "references")
                "gi" '(omnisharp-find-implementations :which-key "implementation"))
            (add-to-list 'company-backends 'company-omnisharp)
            (add-hook 'csharp-mode-hook 'omnisharp-mode)))

    ;; Dockerfile mode
    (use-package dockerfile-mode
        :ensure t
        :mode "Dockerfile\\'")

    ;; Web Mode
    (use-package web-mode
        :ensure t
        :mode "\\.erb\\'" "\\.html?\\'" "\\.ftl\\'")

    (use-package elm-mode
        :ensure t
        :mode "\\.elm\\'")

    ;; PHP
    (use-package php-mode
        :ensure t
        :mode "\\.php\\'")

    ;; Javascript
    (use-package json-mode
        :ensure t
        :mode "\\.json\\'")

    (use-package js2-mode
        :ensure t
        :mode "\\.js\\'" "\\.js\\.erb\\'"
        :config
        (add-hook 'js2-mode-hook
            (lambda ()
                (setq js2-mode-show-parse-errors nil)
                (setq js2-mode-show-strict-warnings nil)
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
                (push '("=>" . ?⇒) prettify-symbols-alist)
                (push '("->" . ?→) prettify-symbols-alist))))

    ;; Js2-refactor
    ;; (use-package js2-refactor
    ;;     :ensure t
    ;;     :diminish js2-refactor-mode
    ;;     :hook js2-mode
    ;;     :after js2-mode)

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
        :hook (css-mode sass-mode scss-mode less-css-mode json-mode))

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
        :defer t
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
        :diminish smartparens-mode)

    ;; Modeline
    (use-package powerline
        :disabled t
        :ensure t
        :config
        (setq powerline-gui-use-vcs-glyph 't)
        (setq powerline-default-separator 'contour)

        (use-package powerline-evil
            :ensure t
            :config
            (setq powerline-evil-tag-style 'verbose)
            (powerline-evil-center-color-theme)))

    (use-package smart-mode-line
        :ensure t
        :init
        (setq sml/theme 'dark)
        :config
        (sml/setup))

    (use-package telephone-line
        :disabled t
        :ensure t
        :init
        (setq telephone-line-evil-use-short-tag t)
        :config
        (telephone-line-mode 1))

    ;; Undo Tree Mode
    (use-package undo-tree
        :ensure t
        :diminish undo-tree-mode
        :diminish undo-tree-visualizer-mode
        :diminish undo-tree-visualizer-selection-mode
        :config
        (global-undo-tree-mode)))

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

(provide 'init)
;;; init.el ends here
