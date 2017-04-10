;;; kelsin-helm.el --- Helm Settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Christopher Giroir

;; Author: Christopher Giroir <kelsin@valefor.com>
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

(require 'helm)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)

(helm-mode 1)
(helm-flx-mode +1)
(helm-fuzzier-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
  helm-move-to-line-cycle-in-source     nil ; move to end or beginning of source when reaching top or bottom of source.
  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
  helm-ff-file-name-history-use-recentf t
  helm-mode-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-recentf-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-locate-fuzzy-match t
  helm-M-x-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-apropos-fuzzy-match t
  helm-lisp-fuzzy-completion t)

;; Key Bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(provide 'kelsin-helm)
;;; kelsin-helm.el ends here
