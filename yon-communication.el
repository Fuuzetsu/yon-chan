;; yon-communication.el --- Communication functions for yon-chan

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

;; This file contains various communication functions used throughout yon-chan

;;; Code:

(require 'yon-utils)

(defun yon-get-json-from-current-buffer ()
  "Kill the current buffer and return the (presumably) JSON
encoded string that was in it."
  (let ((json nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun yon-parse-json (json)
  "Return parse tree for the JSON string."
  (json-read-from-string json))

(defun yon-get-and-parse-json ()
  "Return the parse tree for the JSON string in the current
buffer. The current buffer is killed as a side-effect."
  (yon-parse-json (yon-get-json-from-current-buffer)))

(provide 'yon-communication)
;;; yon-communication.el ends here
