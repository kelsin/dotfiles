;;; kelsin-jekyll.el --- Jekyll Blog Mode

;; Copyright (C) 2014  Christopher Giroir

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
(require 'poly-markdown)

(defcustom  pm-inner/yaml
  (pm-hbtchunkmode "yaml"
                   :font-lock-narrow t
                   :mode 'yaml-mode
                   :head-mode 'yaml-mode
                   :tail-mode 'yaml-mode
                   :head-reg "^---\n"
                   :tail-reg "^---\n\n")
  "Yaml Header Chunk"
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/jekyll
  (pm-polymode-multi-auto "jekyll"
                   :hostmode 'pm-host/markdown
                   :innermodes '(pm-inner/yaml)
                   :auto-innermode 'pm-inner/markdown
                   :init-functions '(poly-markdown-remove-markdown-hooks))
  "Jekyll typical polymode."
  :group 'polymodes
  :type 'object)

(define-polymode poly-jekyll-mode pm-poly/jekyll)

(add-to-list 'auto-mode-alist '("\\.md" . poly-jekyll-mode))

(provide 'kelsin-jekyll)
;;; kelsin-jekyll.el ends here
