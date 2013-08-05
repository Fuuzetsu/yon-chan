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


(defun yon-make-quote-buttons ()
  (let* ((start (string-match "<a href=\".*?\" class=\"quotelink\">"
                              (yon-get-line-content)))
         (op "<a href\"")
         (ed  "</a>"))
    (when start
      (let* ((startn (+ start (line-beginning-position)))
             (end (+ (yon-get-closing-point
                      (yon-get-section startn (point-max)) ed)
                     startn)))
        (save-excursion
          (goto-char startn)
          (delete-char (+ 1 (length op)))
          (goto-char (- end (+ 1 (length op))))
          (delete-backward-char (length ed))
          (goto-char startn)
          (zap-to-char 1 (string-to-char "\""))
          (delete-char (length " class=\"quotelink\">"))
          (insert (current-kill 0))
          (delete-backward-char 1)
          (insert " ")
          (goto-char startn)
          (let* ((split-cont (split-string
                              (yon-get-section
                               startn
                               (- end (+ (length op)
                                         (length " class=\"quotelink\">")
                                         (length ed)
                                         1))) ;; space
                              " "))
                 (link (split-string (car split-cont) "#p"))
                 (res-start (string-match "/res/" (car link))))
            (progn
              (delete-char (+ (length (car split-cont))
                              (length (cadr split-cont))
                              1))
              (if res-start
                  (progn
                    (goto-char startn)
                    (lexical-let* ((board
                                    (with-temp-buffer
                                      (insert (car link))
                                      (goto-char (point-min))
                                      (zap-to-char 2 (string-to-char "/"))
                                      (erase-buffer)
                                      (insert (current-kill 0))
                                      (goto-char (point-min))
                                      (delete-char 1)
                                      (goto-char (point-max))
                                      (delete-backward-char 1)
                                      (buffer-string)))
                                   (thread
                                    (with-temp-buffer
                                      (insert (car link))
                                      (goto-char (point-min))
                                      (delete-char (+ 2
                                                      (length board)
                                                      (length "res/")))
                                      (buffer-string)))
                                   (post (cadr link)))
                      (let ((kmap (make-sparse-keymap)))
                        (define-key kmap (kbd "<return>")
                          (lambda ()
                            (interactive)
                            (yon-browse-thread
                             (switch-to-buffer-other-window
                              (generate-new-buffer
                               (concat "*yon-chan-/" board "/-" thread)))
                             board thread)))
                        (define-key kmap (kbd "a")
                          (lambda ()
                            (interactive)
                            (yon-browse-thread
                             (with-current-buffer
                                 (rename-buffer (concat "*yon-chan-/" board "/-" thread))
                               (current-buffer))
                             board thread)))
                        (insert-text-button (yon-strip-newlines (cadr split-cont))
                                            'face 'yon-face-post-number-link
                                            'keymap kmap))))
                (if (equal 1 (length link)) ;; simple link such as >>>/a/
                    (lexical-let ((board (car link))
                                  (kmap (make-sparse-keymap)))
                      (define-key kmap (kbd "<return>")
                        (lambda ()
                          (interactive)
                          (yon-browse-board-catalog
                           (switch-to-buffer-other-window
                            (generate-new-buffer
                             (concat "*yon-chan-/" board "/*")))
                           (substring board 1 (- (length board) 1)))))
                      (define-key kmap (kbd "a")
                        (lambda ()
                          (interactive)
                          (yon-browse-board-catalog
                           (with-current-buffer
                               (rename-buffer (concat "*yon-chan-/" board "/*"))
                             (current-buffer))
                           (substring board 1 (- (length board) 1)))))
                      (insert-text-button (yon-strip-newlines (cadr split-cont))
                                          'face 'yon-face-post-number-link
                                          'keymap kmap)
                      )
                  (lexical-let ((thread (car link))
                                (post (cadr link))
                                (kmap (make-sparse-keymap))
                                (board (with-current-buffer (buffer-name)
                                         yon-current-board)))
                    (define-key kmap (kbd "<return>")
                      (lambda ()
                        (interactive)
                        (yon-jump-to-local-post (string-to-number post))))
                    (insert-text-button (yon-strip-newlines (cadr split-cont))
                                        'face 'yon-face-post-number-link
                                        'keymap kmap)))))))))))


(defun yon-process-post (body)
  (let* ((cleaned (yon-clean-html-string body)))
    cleaned))


(provide 'yon-rendering)
;;; yon-rendering.el ends here
