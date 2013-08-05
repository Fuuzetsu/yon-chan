;;; yon-rendering.el --- Rendering and appearance function for yon-chan

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

;; This file contains various face and rendering functions used throughout
;; yon-chan

;;; Code:

(require 'yon-structures)

(defface yon-face-greentext
  '((default)
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "green3")
    (((class color) (min-colors 16) (background dark))  :foreground "green3")
    (((class color)) :foreground "green"))
  "Basic greentext face for all the implications we can imply."
  :group 'yon-chan)

(defface yon-face-prettyprint
  '((default)
    (((class color) (min-colors 16) (background light)) :foreground "grey21")
    (((class color) (min-colors 16) (background dark))  :foreground "pink2")
    (((class color)) :foreground "grey21"))
  "Basic face for prettyprint blocks. Colour picked aribtrarily."
  :group 'yon-chan)

(defface yon-face-post-author
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "purple4")
    (((class color) (min-colors 16) (background dark))  :foreground "purple2")
    (((class color)) :foreground "purple"))
  "Basic face for the post author."
  :group 'yon-chan)

(defface yon-face-post-subject
  '((default (:inherit header-line :weight bold))
    (((class color)) :foreground "sea green"))
  "Basic face for the post subject."
  :group 'yon-chan)

(defface yon-face-post-number
  '((default :weight bold)
    (((class color)) :foreground "red3"))
  "Basic face for the post number."
  :group 'yon-chan)

(defface yon-face-post-number-link
  '((default :weight bold :underline t)
    (((class color)) :foreground "red3"))
  "Basic face for the post number."
  :group 'yon-chan)

(defface yon-face-deadlink
  '((default :strike-through t)
    (((class color)) :foreground "red2"))
  "Basic face for dead cross-links."
  :group 'yon-chan)

(defmacro $. (f g x)
  `(,f (,g ,x)))

(defun yon-apply-faces (text)
  ($. yon-apply-quotelinks
      yon-apply-deadlinks
      ($. yon-apply-greentext
          yon-apply-prettyprint text)))

(defun yon-render (buffer proc obj)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (funcall proc obj)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun yon-render-catalog (catalog)
  (mapc 'yon-render-catalog-page catalog))

(defun yon-apply-deadlinks (text)
  "Checks each line for deadlink replacement."
  (save-excursion
    (lexical-let ((board (with-current-buffer (current-buffer)
                           yon-current-board)))
      (with-temp-buffer
        (set (make-local-variable 'yon-current-board) board)
        (insert text)
        (goto-char (point-min))
        (cl-loop until (eobp) do (yon-possibly-colorify-line-by-tags
                                  "<span class=\"deadlink\">" "</span>"
                                  'yon-face-deadlink t)
                 (forward-line 1))
        (buffer-string)))))

(defmacro yon-apply-face-between-regex (text face oprx endrx nl)
  "Macro that generalises face application between regex delimiters."
  `(save-excursion
    (lexical-let ((board (with-current-buffer (current-buffer)
                           yon-current-board)))
      (with-temp-buffer
        (set (make-local-variable 'yon-current-board) board)
        (insert ,text)
        (goto-char (point-min))
        (cl-loop until (eobp) do (yon-possibly-colorify-line-by-tags
                                  ,oprx ,endrx ,face ,nl)
                 (forward-line 1))
        (buffer-string)))))

(defun yon-apply-greentext (text)
  "Checks each line for greentext replacement."
  (yon-apply-face-between-regex
   text 'yon-face-greentext "<span class=\"quote\">" "</span>" t))

(defun yon-apply-prettyprint (text)
  (yon-apply-face-between-regex
   text 'yon-face-prettyprint "<pre class=\"prettyprint\">" "</pre>" nil))

(defun yon-apply-quotelinks (text)
  (save-excursion
    (lexical-let ((board (with-current-buffer (current-buffer)
                           yon-current-board)))
      (with-temp-buffer
        (set (make-local-variable 'yon-current-board) board)
        (insert text)
        (goto-char (point-min))
        (cl-loop until (eobp) do (yon-make-quote-buttons) (forward-line 1))
        (buffer-string)))))


(defun yon-render-catalog-page (page)
  (dolist (post page)
    (setf (yon-post-renderpos post) (point))
    (insert (concat (yon-apply-faces (yon-format-post post)) "\n"))))

(defun yon-render-thread (posts)
  (let ((op (car posts))
        (replies (cdr posts)))
    (setf (yon-post-renderpos op) (point))
    (insert (yon-apply-faces (concat (yon-format-post op))) "\n")
    (dolist (post replies)
      (setf (yon-post-renderpos post) (point))
      (insert
       (concat (yon-apply-faces
                (replace-regexp-in-string "^" "    "
                                          (yon-format-post post))) "\n")))))

(provide 'yon-rendering)
;;; yon-rendering.el ends here
