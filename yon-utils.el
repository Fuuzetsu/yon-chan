;; yon-utils.el --- Util functions for yon-chan

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

;; This file contains various utility functions used throughout yon-chan

;;; Code:

(defun yon-strip-newlines (body)
  "Return a copy of the string BODY with all newline characters
removed."
  (replace-regexp-in-string "\n" "" body))

(defun yon-clean-html-string (body)
  "Return a copy of the string BODY with certain HTML encoded
characters and tags removed."
  (let* ((replace-list (list '("&#039;" . "'")
                             '("&gt;" . ">")
                             '("&lt;" . "<")
                             '("&quot;" . "\"")
                             '("&amp;" . "&")
                             '("<br>" . "\n")
                             '("<wbr>" . "")))
         (replacer (lambda (x y)
                     (replace-regexp-in-string
                      (car y) (cdr y) x))))
    (reduce replacer (cons body replace-list))))

(defun yon-get-line-content ()
  "Return the contents of the current line."
  (save-excursion
    (buffer-substring (line-beginning-position)
                      (line-beginning-position 2))))

(defun yon-get-closing-point (bufstr close)
  "Return the point after which the string CLOSE appears in the
string BUFSTR."
  (+ (string-match close bufstr)
     (length close)))

(defun yon-get-section (start end)
  "Return the string that is between START and END points."
  (save-excursion
    (buffer-substring start end)))

(defun yon-replace-string-section (original replace start end)
  "Return a new string that is a copy of ORIGINAL with the
section from START to END replaced with the string REPLACE."
  (concat (substring original 0 start)
          replace
          (substring original end)))

(defun yon-elem (alst key &optional default)
  "Return value from the alist ALST with a default value of
DEFAULT if KEY is not present. The default return value is nil."
  (lexical-let ((elem (cdr (assoc key alst))))
    (if elem
        elem
      default)))

(provide 'yon-utils)
;;; yon-utils.el ends here
