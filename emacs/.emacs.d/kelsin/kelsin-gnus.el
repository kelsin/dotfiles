;;; kelsin-gnus.el --- Options for Gnus

;; Copyright (C) 2014  Christopher Giroir

;; Author: Christopher Giroir <cgiroir@berklee.edu>
;; Keywords: mail

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

(setq gnus-select-method '(nnimap "imap.gmail.com")
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465)

(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-sort-gathered-threads-function 'gnus-thread-sort-by-number)

(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

(provide 'kelsin-gnus)
;;; kelsin-gnus.el ends here
