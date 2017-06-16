;;; kelsin-modes.el --- Custom installed modes

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

;; This file loads up different modes installed into ~/emacs.d/lisp or that come with emacs

;;; Code:

;; Polymode
;; (require 'kelsin-jekyll)

;; EditorConfig
(editorconfig-mode 1)

;; Recently Used Files
(autoload 'recentf-open-files "recentf" t)

;; Ido
(setq ido-confirm-unique-completion t
  ido-enable-flex-matching t
  ido-everywhere t
  ido-max-prospects 6
  ido-use-faces nil
  ido-use-virtual-buffers t
  ido-show-dot-for-dired t)
(ido-mode 1)
(flx-ido-mode 1)

;; Smex
(smex-initialize)

;; Projectile
(projectile-global-mode)
(define-key global-map (kbd "C-x f") 'projectile-find-file)
; (persp-mode)
; (require 'persp-projectile)

;; FlyCheck
(require 'flycheck)
(global-flycheck-mode)

;; Git gutter fringe+
(require 'git-gutter-fringe+)

;; Haskell
(require 'haskell-mode)
(setq haskell-font-lock-symbols t)

;; Markdown
(add-hook 'markdown-mode-hook
  (lambda ()
    (fset 'markdown-links
      (lambda
        (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([19 60 97 13 19 104 114 101 102 61 34 13 67108896 19 34 13 left 23 18 60 97 13 91 93 40 25 41 67108896 19 62 13 23 67108896 19 60 13 left 23 18 91 93 13 right 25 19 60 47 97 62 13 backspace backspace backspace backspace] 0 "%d"))
          arg)))
    (local-set-key (kbd "C-c C-l") 'markdown-links)))

;; Ruby Files
(add-to-list 'auto-mode-alist '("\\.csv\\.csvbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.god\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))

;; Cucumber
(require 'feature-mode)
(setq feature-default-language "en")

;; Magit - Required for modeline
(autoload 'magit-get-current-branch "magit" t)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Java Settings
(require 'kelsin-java)

;; Javascript
(add-hook 'json-mode-hook
  (lambda ()
    (setq js-indent-level 2)))
(add-hook 'js2-mode-hook
  (lambda ()
    (setq js-indent-level 2)
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb\\'" . js2-mode))

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
(add-hook 'flycheck-mode-hook #'kelsin/use-eslint-from-node-modules)

;; Rainbow
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'less-css-mode-hook 'rainbow-mode)

;; Scss
(setq scss-compile-at-save nil)

;; Javadoc Help
(autoload 'javadoc-lookup       "javadoc-help" "Look up Java class in Javadoc."   t)
(autoload 'javadoc-help         "javadoc-help" "Open up the Javadoc-help menu."   t)
(autoload 'javadoc-set-predefined-urls  "javadoc-help" "Set pre-defined urls."    t)

;; nxml formatting
(setq nxml-sexp-element-flag t
  nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.adp" . nxml-mode))
(add-hook 'nxml-mode-hook
  (lambda ()
    (setq indent-tabs-mode 't)
    (setq tab-width 2)))

;; vue
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))

;; jsx
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; Js2-refactor
(add-hook 'js2-mode-hook 'js2-refactor-mode)

;; Tern
(setq tern-command '("tern" "--no-port-file"))

(provide 'kelsin-modes)
;;; kelsin-modes.el ends here
