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

(defface yon-face-greentext
  '((default)
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "green3")
    (((class color) (min-colors 16) (background dark))  :foreground "green3")
    (((class color)) :foreground "green3"))
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



(provide 'yon-rendering)
;;; yon-rendering.el ends here
