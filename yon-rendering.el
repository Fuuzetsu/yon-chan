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
(require 'yon-utils)

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
  (let ((r (yon-make-quote-buttons text)))
    (if r (yon-apply-quotelinks r) text)))

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

(defun yon-possibly-colorify-line-by-tags
  (opregex endregex colourface &optional newline)
  "Applies colourface to anything between opregex and endregex.
If newline is non-nil, newlines in the matching text will be removed."
  (let ((start (string-match opregex (yon-get-line-content)))
        (op opregex)
        (ed endregex))
    (when start
      (let* ((startn (+ start (line-beginning-position)))
             (end (+ (yon-get-closing-point
                      (yon-get-section startn (point-max)) ed)
                     startn)))
        (save-excursion
          (goto-char startn)
          (delete-char (length op))
          (goto-char (- end (length op)))
          (delete-backward-char (length ed))
          (goto-char startn)
          (let ((cont (yon-get-section startn (- end
                                                 (+ (length op)
                                                    (length ed))))))
            (progn
              (delete-char (length cont))
              (if newline
                  (insert (propertize (yon-strip-newlines cont)
                                      'face colourface))
                (insert (propertize cont
                                    'face colourface))))))))))


(defun yon-extract-quote-link (s)
  "Splices out contents of a href quotelink string into a pair"
  (let* ((rx "<a href=\"\\(.+?\\)\" class=\"quotelink\">\\(.+?\\)</a>")
         (left (replace-regexp-in-string rx "\\1" s))
         (right (replace-regexp-in-string rx "\\2" s)))
    (cons left right)))

(defun yon-make-quote-buttons (s)
  (let* ((lrx "<a href=\".+?\" class=\"quotelink\">.+</a>")
         (start (string-match lrx s))
         (ed "</a>"))
    (when start
      (lexical-let* ((end (+ (length ed) (string-match ed s start)))
                     (href (substring s start end))
                     (split (yon-extract-quote-link href))
                     (linkleft (car split))
                     (linksplit (split-string linkleft "#p"))
                     (res-start (string-match "/res/" linkleft))
                     (board-only (string-match "/.+?/" linkleft))
                     (kmap (make-sparse-keymap))
                     (text-str (with-temp-buffer
                                 (insert-text-button
                                  (cdr split)
                                  'face 'yon-face-post-number-link
                                  'keymap kmap)
                                 (buffer-string)))
                     (kbind (lambda (key body)
                              (lexical-let ((b body))
                                (define-key kmap (kbd key)
                                  (lambda ()
                                    (interactive)
                                    (apply b)))))))
        (if res-start
            (lexical-let* ((board (substring linkleft 1 res-start))
                           (thread (substring linkleft (+ (length "/res/") res-start)))
                           (post (cdr linksplit))
                           (bname (concat "*yon-chan-/" board "/-" thread)))
              (funcall kbind "<return>"
                       '(yon-browse-thread
                         (switch-to-buffer-other-window
                          (generate-new-buffer bname))
                         board thread))
              (funcall kbind "a"
                       '(yon-browse-thread
                         (with-current-buffer (rename-buffer bname)
                           (current-buffer))
                         board thread))
              (yon-replace-string-section s text-str start end))
          (if board-only ;; simple link such as >>>/a/
              (lexical-let* ((board (substring linkleft 1 (- (length linkleft) 1)))
                             (bname (concat "*yon-chan-/" board "/*")))
                (funcall kbind "<return>"
                         '(yon-browse-board-catalog
                           (switch-to-buffer-other-window
                            (generate-new-buffer
                             bname))
                           board))
                (funcall kbind "a"
                         '(yon-browse-board-catalog
                           (with-current-buffer
                               (rename-buffer bname)
                             (current-buffer))
                           board))
                (yon-replace-string-section s text-str start end))
            (lexical-let* ((thread (car linksplit))
                           (post (string-to-number (cadr linksplit)))
                           (board (with-current-buffer (buffer-name)
                                    yon-current-board)))
              (funcall kbind "<return>" `(yon-jump-to-local-post ,post))
              (yon-replace-string-section s text-str start end))))))))

(defun yon-process-post (body)
  (yon-clean-html-string body))


(provide 'yon-rendering)
;;; yon-rendering.el ends here
