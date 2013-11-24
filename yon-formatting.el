;;; yon-formatting.el --- Post formatting functions for yon-chan

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

;; This file contains various formatting functions used throughout yon-chan

;;; Code:

;;; Formatting

(defun yon-format-post (post)
  "Returns a formatted string representation of a post"
  (format "%s\n%s\n"
          (yon-format-post-header post)
          (yon-format-post-comment post)))

(defun yon-format-post-header (post)
  "Returns a formatted string representation of a post header.
The header consists of the subject, author, timestamp, and post number."
  (lexical-let ((items (list (yon-format-post-subject post)
                             (yon-format-post-author post)
                             (yon-format-post-timestamp post)
                             (yon-format-post-number post)
                             (yon-format-post-image post))))
    ;; Some header items could be blank (such as the image filename), so
    ;; remove all nil values.
    (json-join  (remq nil items) " - ")))

(defun yon-format-post-comment (post)
  "Returns a processed comment string."
  (yon-process-post (yon-post-comment post)))

(defun yon-format-post-subject (post)
  "Returns a propertized, html-cleaned subject string."
  (propertize
    (yon-clean-html-string (yon-post-subject post))
    'face 'yon-face-post-subject))

(defun yon-format-post-author (post)
  "Returns a propertized, html-cleaned author string."
  (propertize
    (yon-clean-html-string (yon-post-author post))
    'face 'yon-face-post-author))

(defun yon-format-post-timestamp (post)
  "Returns an html-cleaned timestamp string."
  (yon-clean-html-string (yon-post-timestamp post)))

(defun yon-format-post-image (post)
  (if (yon-post-extension post)
      (lexical-let ((kmap (make-sparse-keymap))
                    (board (yon-current-buffer-board))
                    (extension (yon-post-extension post))
                    (filename (yon-post-filename post))
                    (new-filename (format "%d" (yon-post-new-filename post))))
        (define-key kmap (kbd "<return>")
          (lambda ()
            (interactive)
            (yon-display-image-other-window
             (concat "http://images.4chan.org/"
                     board
                     "/src/"
                     new-filename
                     extension)
             (concat filename extension))))
        (with-temp-buffer
          (insert-text-button (concat filename extension)
                              'keymap kmap)
          (buffer-string)))
    ""))

(defun yon-format-post-number (post)
  "Returns the post number. Clickable if it's a thread OP."
  (lexical-let* ((op (equal 0 (yon-post-replyto post)))
                 (kmap (make-sparse-keymap))
                 (board (yon-current-buffer-board))
                 (post-number (number-to-string (yon-post-number post)))
                 ;; Need to rebind in lexical scope
                 (post post)
                 (browse (lambda ()
                           (interactive)
                           (yon-browse-thread-other-window post))))
    (define-key kmap (kbd "<return>") browse)
    (define-key kmap (kbd "a") browse)
    (if (and op board)
        (with-temp-buffer
          (insert-text-button post-number
                              'face 'yon-face-post-number-link
                              'keymap kmap)
          (buffer-string))
      (propertize post-number 'face 'yon-face-post-number))))

(provide 'yon-formatting)
;;; yon-formatting.el ends here
