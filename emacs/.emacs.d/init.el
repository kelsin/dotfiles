;;; init.el --- Init File

;; NOTE: early-init.el is now generated from readme.org.
;; Please edit that file instead

;; User
(setq user-full-name "Christopher Giroir"
      user-mail-address "kelsin@valefor.com")

;; Restore gc settings after init
(add-hook 'after-init-hook '(lambda ()
                              ;; restore after startup
                              (setq gc-cons-threshold 16777216
                                    gc-cons-percentage 0.1)))

;; Assume :straight t for all use-package macros
(setq straight-use-package-by-default t)

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;;; Generic Settings

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

;; Mac Settings
(if (and
     (eq system-type 'darwin)
     (featurep 'ns))
    (setq ns-alternate-modifier 'super
	  ns-command-modifier 'meta
	  ns-extended-platform-support-mode t
	  ns-pop-up-frames nil
	  ns-use-qd-smoothing nil))

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Yellow bar cursor
(add-to-list 'default-frame-alist '(cursor-type . bar))
(add-to-list 'initial-frame-alist '(cursor-type . bar))
(set-cursor-color "yellow")

;; Ansi colors for compilation output
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Ansi colors in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Don't warn me about loading large files (TAGS files)
(setq large-file-warning-threshold nil)

;; Never use dialog windows
(setq use-dialog-box nil)

;; Set Shell to bash
(setq shell-file-name "/bin/bash")

;; Set exec-path
;; (setenv "PATH" (concat "/usr/local/share/dotnet:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append `(,(expand-file-name "~/.nodenv/shims")
                          ,(expand-file-name "~/.rbenv/shims")
                          ,(expand-file-name "~/.pyenv/shims")
                          ,(expand-file-name "~/Library/pnpm")
                          "/usr/local/bin") exec-path))

(defun kelsin/add-to-path (item)
  "Add an item to both the PATH environment variable and the emacs exec-path variable"
  (interactive)
  (setenv "PATH" (concat (expand-file-name item) ":" (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") path-separator)))
(kelsin/add-to-path "~/.nodenv/shims")
(kelsin/add-to-path "~/.rbenv/shims")
(kelsin/add-to-path "~/.pyenv/shims")
(kelsin/add-to-path "/usr/local/bin")

;; Add a lisp folder and my functions package to the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/kelsin/")

;; Don't prompt for compile commands and auto jump to first error
(setq compilation-read-command nil)
(setq compilation-auto-jump-to-first-error 't)
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 10)

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

;; Scrolling
(setq
 scroll-margin 1
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Expiration Date of Buffers
(setq clean-buffer-list-delay-general 1)

;; Diminish Mode
(use-package diminish)

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

;; Set paren style
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Reindent then newline and indent
(global-set-key (kbd "M-RET") 'reindent-then-newline-and-indent)

;; No suspend in terminal
(global-unset-key (kbd "C-z"))

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

;; Visible Bell
(setq visible-bell t)

;; Default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; Default to unified diffs
(setq diff-switches "-u")

;; Make files with #! at the beginning executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; WDired
(setq wdired-allow-to-change-permissions 't)

;; Disable VC
(setq vc-handled-backends nil)

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

;;; Theme and Font
(setq-default line-spacing 3)
(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-18"))
;; (use-package base16-theme
;;   :config
;;   (load-theme 'base16-default-dark t))
;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-vivendi))
(use-package kaolin-themes
  :after all-the-icons
  :config
  (load-theme 'kaolin-aurora t)
  (kaolin-treemacs-theme))
;; (add-to-list 'custom-theme-load-path "~/src/blizzard-colors/emacs")
;; (load-theme 'blizzard 't)
;; (use-package solarized-theme
;;     :config
;;     (load-theme 'solarized-dark t))
;; (use-package zenburn-theme
;;     :config
;;     (load-theme 'zenburn t))
;; (load-theme 'airbnb 't)
;; (use-package spacemacs-theme
;;     :config
;;     (load-theme 'spacemacs-dark t))

;; Mode Line
(use-package smart-mode-line
  :init
  (setq sml/theme 'dark)
  :config
  (sml/setup))

;; Which Key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; General
(use-package general
  :config
  (general-evil-setup)

  (defun kelsin/devdocs (query)
    "Look up the provided search query in devdocs.io"
    (interactive "sQuery: ")
    (let ((url "http://devdocs.io/#q="))
      (browse-url (concat url (or query "")))))

  (defun kelsin/devdocs-at-point ()
    "Look up the word under cursor in devdocs.io"
    (interactive)
    (let ((url "http://devdocs.io/#q=")
          (query (or (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'symbol))
                     "")))
      (browse-url (concat url query))))

  (general-define-key
   :states '(normal visual insert)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "b" '(:ignore t :which-key "buffer")
   "br" '(revert-buffer :which-key "revert")
   "c" '(:ignore t :which-key "customize")
   "ca" '(customize-apropos :which-key "apropos")
   "cl" '(display-line-numbers-mode :which-key "line numbers")
   "ch" '(hl-line-mode :which-key "highlight current line")
   "d" '(dired-jump :which-key "dired")
   "g" '(:ignore t :which-key "go to")
   "gd" '(lsp-find-definition :which-key "definition")
   "gr" '(lsp-find-references :which-key "references")
   "ge" '(next-error :which-key "next error")
   "l" '(kelsin/devdocs-at-point :which-key "devdocs at point")
   "L" '(kelsin/devdocs :which-key "devdocs query")))

;; Needed by other packages to provide good fuzzy finding
(use-package flx)

;; Ability to write out html versions of emacs buffers
(use-package htmlize)

;; Page Break Lines
(use-package page-break-lines)

;; Dired
(use-package dired-x
  :straight nil
  :config
  ;; Dired Searches only use filename
  (setq dired-isearch-filenames t)
  (setq dired-use-ls-dired nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$\\|^\\.git$"))
  (add-to-list 'dired-omit-extensions ".meta")
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

;; Uniquify
(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'post-forward))

;; Global HL Mode Line
(use-package hl-line
  :config
  (global-hl-line-mode))

;; Rainbow
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (css-mode sass-mode scss-mode less-css-mode json-mode))

;; Auto revert
(global-auto-revert-mode 1)

;; Prettify Symbols
(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Undo Tree Mode
(use-package undo-tree
  :diminish undo-tree-mode
  :diminish undo-tree-visualizer-mode
  :diminish undo-tree-visualizer-selection-mode
  :config
  (global-undo-tree-mode))

;; EditorConfig
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Fancy Kill Ring
(use-package browse-kill-ring
  :bind ( :map evil-insert-state-map
               ("M-y" . browse-kill-ring)))

;; Evil
(use-package evil
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
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; Modes to use emacs mode in
  (add-to-list 'evil-emacs-state-modes 'nav-mode)
  (add-to-list 'evil-emacs-state-modes 'easy-jekyll-mode)
  (add-to-list 'evil-emacs-state-modes 'neotree-mode))

;; Evil Collection
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; Evil Matchit
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

;; Evil Numbers
(use-package evil-numbers
  :after evil)

;; Evil Commentary
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;; Evil Surround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Evil Magit
(use-package evil-magit
  :after '(evil magit))

;; Org Mode
(use-package org
  :init
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/todo.org")
  (setq org-log-done t)
  (setq org-startup-folded nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-link-file-path-type 'relative)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
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
  (use-package ob-restclient)
  (use-package ox-reveal)

  (setq org-log-refile t)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  ;(setq org-agenda-files '("~/org" "~/blizzard/src/org"))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  ;; (defvar kelsin/org-capture-file-blizzard-todo
  ;;   "~/blizzard/src/org/todo.org"
  ;;   "File to use when saving new Blizzard TODO items with Org Capture.")
  ;; (defvar kelsin/org-capture-file-todo
  ;;   "~/org/todo.org"
  ;;   "File to use when saving new personal TODO items with Org Capture.")

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

  ;; (setq org-todo-keyword-faces
  ;;       '(("TODO" . blizzard-bryellow)
  ;;         ("NEXT" . blizzard-yellow)
  ;;         ("DONE" . blizzard-brgreen)
  ;;         ("CANCELLED" . blizzard-white)
  ;;         ("WAITING" . blizzard-brred)))

  ;; (setq org-todo-state-tags-triggers
  ;;       (quote (("CANCELLED" ("CANCELLED" . t))
  ;;               ("WAITING" ("WAITING" . t))
  ;;               (done ("WAITING"))
  ;;               ("TODO" ("WAITING") ("CANCELLED"))
  ;;               ("NEXT" ("WAITING") ("CANCELLED"))
  ;;               ("DONE" ("WAITING") ("CANCELLED")))))

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
   "o." '(org-time-stamp :which-key "timestamp")
   "o[" '(org-agenda-file-to-front :which-key "add to agenda")
   "o]" '(org-remove-file :which-key "remove from agenda")
   "od" '(org-deadline :which-key "deadline")
   "oe" '(org-export-dispatch :which-key "export")
   "oo" '(org-open-at-point :which-key "open")
   "op" '(org-priority :which-key "priority")
   "os" '(org-schedule :which-key "schedule")
   "ot" '(org-todo :which-key "todo"))

  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
  ;;         (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

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

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Automatically tangle our Emacs.org config file when we save it
(defun kelsin/org-babel-tangle-config ()
  (when (string-suffix-p "/.emacs.d/readme.org"
                         (buffer-file-name))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'kelsin/org-babel-tangle-config)))

;; Magit
(use-package magit
  :bind ("C-c i" . magit-status))

;; Magit
(use-package easy-jekyll
  :config
  (setq easy-jekyll-basedir "~/src/mx.kelsin.net/"))

;; Ivy
(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist
    '((swiper . ivy--regex-plus)
          (counsel-ag-function . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-x r b" . counsel-bookmark)
  ("C-c C-a" . counsel-apropos)
  ("C-c C-g" . counsel-ag)
  :init
  (setq counsel-rg-base-command "rg -M 120 -S --no-heading --line-number --color never %s .")
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "f" '(:ignore t :which-key "File")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")))

(use-package counsel-projectile
  :after (counsel projectile)
  :hook (projectile-mode . counsel-projectile-mode)
  :config
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default "D"))))

;;; File Handling

;; All The Icons
(use-package all-the-icons
  :config
  (use-package all-the-icons-dired
    :diminish all-the-icons-dired-mode
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(use-package neotree
  :bind ("<f9>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
    neo-smart-open t
    neo-window-width 30
    neo-auto-indent-point t
    neo-autorefresh t
    neo-force-change-root t))

;; Bookmarks
(use-package bookmark
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-sort-flag 1))

;; Recentf
(use-package recentf
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

;; Projectile
(use-package projectile
  :demand t
  :bind
  (:map projectile-mode-map
    ("C-c p" . projectile-command-map)
    ("C-c p g" . counsel-projectile-rg))
  :config
  (add-to-list 'projectile-globally-ignored-directories "object_metadata")
  (add-to-list 'projectile-globally-ignored-directories "minerva_metadata")
  (setq projectile-generic-command
    "find . -type f ! -ipath '*/object_metadata' ! -ipath '*/minerva_metadata' -print0")
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

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-enable-file-watchers nil)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company
  :commands company-lsp)

;;; Filetypes

;; Restclient
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient)))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

;; Dockerfile mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Handlebars
(use-package handlebars-mode)

;; Auctex
(use-package tex-mode :straight auctex)

;; Ember Mode
(use-package ember-mode
  :commands ember-mode)

;; Pug
(use-package pug-mode
  :mode "\\.pug\\'" "\\.jade\\'")

;; Plantuml mode
(use-package plantuml-mode
  :mode "\\.uml\\'")

;; Yaml
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  ;; Don't edit whitespace in yaml files by default
  (add-hook 'yaml-mode-hook
            (lambda ()
              (remove-hook 'write-file-functions 'delete-trailing-whitespace t))))

;; Nginx mode
(use-package nginx-mode
  :mode "\\.conf\\'")

;; Protobuf mode
(use-package protobuf-mode
  :mode "\\.proto\\'" "\\.schema\\'")

;; Terraform
(use-package terraform-mode
  :mode "\\.tf\\'"
  :config
  (use-package company-terraform))

;; Graphviz
(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

;;; Languages

(add-hook 'python-mode-hook (lambda () (remove-hook 'write-file-functions 'delete-trailing-whitespace t)))
(use-package python-docstring
  :after python
  :hook (python-mode . python-docstring-mode))
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))
(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Lua mode
(use-package lua-mode
  :mode "\\.lua\\'")

;; R
(use-package ess)

;; Groovy mode
(use-package groovy-mode
  :mode "\\.groovy\\'")

;; Vimrc mode
(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

;; Go
(use-package go-mode
  :mode "\\.go\\'")

;; Php
(use-package php-mode
  :mode "\\.php\\'")

;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (setq haskell-font-lock-symbols t))

;; Javascript
(use-package prettier-js
  :hook (json-mode . prettier-js-mode)
  :hook (yaml-mode . prettier-js-mode)
  :hook (js-mode . prettier-js-mode)
  :hook (js2-mode . prettier-js-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package js2-mode
  :mode "\\.[cm]?js\\'" "\\.js\\.erb\\'"
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

(use-package rjsx-mode
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

;; Snippets
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)
    (add-hook 'term-mode-hook
          (lambda()
        (setq yas-dont-activate-functions t)))))

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :defer 1
  :config
  (global-flycheck-mode)

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "F" '(:ignore t :which-key "Flycheck")
   "Fs" '(flycheck-select-checker :which-key "select checker")
   "Fn" '(flycheck-next-error :which-key "next error"))

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

;; Copilot
;(use-package copilot
;  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;  :ensure t
;  :config
;  (add-hook 'prog-mode-hook 'copilot-mode)
;  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

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
