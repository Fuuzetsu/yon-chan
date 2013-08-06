;; yon-actions.el --- Action functions for yon-chan

;; Copyright (C) 2013  David Thompson
;;           (C) 2013  Mateusz Kowalczyk

;; Author: David Thompson <dthompson@member.fsf.org>
;; Keywords: hypermedia, games

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

;; This file contains various actions used throughout yon-chan

;;; Code:

(defun display-image-other-window (url name)
  (lexical-let ((name name))
    (url-retrieve
     (progn (message (concat "Retrieving " url)) url)
     (lambda (status)
       (local-set-key (kbd "Q") 'kill-buffer-and-window)
       (rename-buffer name t)
       ;; Delete HTTP header
       (re-search-forward "\r?\n\r?\n")
       (delete-region (point-min) (point))
       (image-mode)
       (switch-to-buffer-other-window (buffer-name))))))


(provide 'yon-actions)
;;; yon-actions.el ends here
