;;; kelsin-evil.el --- Evil Customizations           -*- lexical-binding: t; -*-

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

;; Main Evil
(use-package evil
  :ensure t
  :bind (
          :map evil-motion-state-map
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
  :diminish evil-commentary-mode
  :diminish undo-tree-mode
  :config
  (progn
    (evil-mode 1)

    ;; Modes to use emacs mode in
    (add-to-list 'evil-emacs-state-modes 'nav-mode)
    (add-to-list 'evil-emacs-state-modes 'magit-mode)
    (add-to-list 'evil-emacs-state-modes 'dired-mode)

    ;; Cursors
    (setq evil-emacs-state-cursor '("blue" hbar))
    (setq evil-normal-state-cursor '("green" hbar))
    (setq evil-visual-state-cursor '("orange" hbar))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))))


;; Evil Matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

;; Evil Commentary
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

;; Evil Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; bind evil-args text objects
;; (define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
;; (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
;; (define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; Window moves
;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Modes to use emacs mode in
;; (add-to-list 'evil-emacs-state-modes 'nav-mode)
;; (add-to-list 'evil-emacs-state-modes 'magit-mode)
;; (add-to-list 'evil-emacs-state-modes 'dired-mode)

;; Cursors
;; (setq evil-emacs-state-cursor '("blue" hbar))
;; (setq evil-normal-state-cursor '("green" hbar))
;; (setq evil-visual-state-cursor '("orange" hbar))
;; (setq evil-insert-state-cursor '("red" bar))
;; (setq evil-replace-state-cursor '("red" bar))
;; (setq evil-operator-state-cursor '("red" hollow))

;; Diminish
;; (diminish 'evil-commentary-mode)
;; (diminish 'undo-tree-mode)

(provide 'kelsin-evil)
;;; kelsin-evil.el ends here
