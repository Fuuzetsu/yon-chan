;;; yon-chan.el --- A 4chan browser for Emacs

;; Copyright (C) 2013  David Thompson

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

;; This mode uses the 4chan API to provide a board and thread browser
;; from within Emacs

;;; Code:

(require 'json)
(require 'cl)

;;; Yon-chan mode

(define-derived-mode yon-chan-mode
  special-mode "Yon-chan"
  "4chan browser.")

;; (defun yon-mode-keys ()
;;   "Set local key defs for yon-mode"
;;   (define-key yon-mode-map "q" 'quit-window)

(global-set-key (kbd "C-c C-r gd") 'yon-apply-deadlinks)
(global-set-key (kbd "C-c C-r gl") 'yon-apply-greenstuff)

;;; Faces

(defface yon-chan-greentext
  '((default)
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "green3")
    (((class color) (min-colors 16) (background dark))  :foreground "green3")
    (((class color)) :foreground "green3"))
  "Basic greentext face for all the implications we can imply.")

(defface yon-chan-poster
  '((default :weight bold)
    (((class color) (min-colors 16) (background light)) :foreground "purple4")
    (((class color) (min-colors 16) (background dark))  :foreground "purple2")
    (((class color)) :foreground "purple"))
  "Basic face for the poster name.")

(defface yon-chan-topic-name
  '((default :weight bold)
    (((class color)) :foreground "brown"))
  "Basic face for the topic name.")

(defface yon-chan-post-number
  '((default :weight bold)
    (((class color)) :foreground "red3"))
  "Basic face for the poster number.")

(defface yon-chan-deadlink
  '((default :strike-through t)
    (((class color)) :foreground "red2"))
  "Basic face for dead cross-links.")

;;; Comment sanitization and processing

(defun yon-clean-html-string (body)
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
  (save-excursion
   (let ((pos-end (line-beginning-position 2)))
     (buffer-substring (line-beginning-position) pos-end))))

(defun yon-get-closing-point (bufstr close)
  (let ((match (string-match close bufstr)))
    (when match
      (+ match (length close)))))

(defun yon-get-section (start end)
  (save-excursion
    (buffer-substring start end)))


;; Interactive for now for testing
(defun yon-possibly-colorify-line-by-tags
  (opregex endregex colourface &optional newline)
  "Least elegant function that will replace quotes with greentext."
  (interactive)
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


;; interactive for testing
(defun yon-apply-deadlinks ()
  "Checks each line for deadlink replacement."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp) do (yon-possibly-colorify-line-by-tags
                              "<span class=\"deadlink\">" "</span>"
                              'yon-chan-deadlink t)
             (forward-line 1))))

;; interactive for testing
(defun yon-apply-greenstuff ()
  "Checks each line for greentext replacement."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp) do (yon-possibly-colorify-line-by-tags
                              "<span class=\"quote\">" "</span>"
                              'yon-chan-greentext t)
             (forward-line 1))))

;; Interactive for now for testing
(defun yon-possibly-greenify-line ()
  "Least elegant function that will replace quotes with greentext."
  (interactive)
  (let ((op "<span class=\"quote\">")
        (ed "</span>")))
  )

(defun yon-strip-newlines (body)
  (replace-regexp-in-string "\n" "" body))

(defun yon-process-post (body)
  (let* ((cleaned (yon-clean-html-string body)))
    cleaned))

;;; 4chan JSON API

(defvar yon-api-url "http://api.4chan.org/")

(defun yon-elem (alst key &optional default)
  "Fetch value from alist with a default value if key is not present."
  (lexical-let ((elem (cdr (assoc key alst))))
    (if elem
        elem
      default)))

(defun yon-get-json-from-current-buffer ()
  (let ((json nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun yon-parse-json (json)
  (json-read-from-string json))

(defun yon-get-and-parse-json ()
  (yon-parse-json (yon-get-json-from-current-buffer)))

;;; Rendering

(defun yon-render (buffer proc json)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (funcall proc json)
    (yon-apply-greenstuff)
    (yon-apply-deadlinks)
    (setq buffer-read-only t)))

(defun yon-render-board (catalog)
  (mapc 'yon-render-page catalog))

(defun yon-render-page (page)
  (mapc 'yon-render-op-post (yon-elem page 'threads)))

(defun yon-render-thread (thread)
  (insert "Thread:\n")
  (mapc 'yon-render-post (yon-elem thread 'posts)))

;; These render functions are gross.
;; Using some quasiquoting and a function to render a template would be better.
;; Input:
;;   `(Subject - ,(yon-elem post 'sub "No subject"))
;; Possible Output:
;;   Subject - No Subject
(defun yon-render-post (post)
  (yon-insert-header post)
  (insert (number-to-string (yon-elem post 'no)))
  (newline)
  (insert (yon-process-post (yon-elem post 'com "")))
  (newline)
  (newline))

(defun yon-render-op-post (post)
  (yon-insert-header post)
  (auto-fill-mode t)
  (insert (yon-process-post (yon-elem post 'com "")))
  (auto-fill-mode nil)
  (newline)
  (newline))

(defun yon-insert-header (post)
  (insert
   (propertize
    (yon-clean-html-string (yon-elem post 'sub "No subject"))
    'face 'yon-chan-topic-name))
  (insert " - ")
  (insert
   (propertize
    (yon-clean-html-string (yon-elem post 'name "No name"))
    'face 'yon-chan-poster))
  (insert " - ")
  (insert (yon-clean-html-string (yon-elem post 'now)))
  (insert " - ")
  (insert
   (propertize
    (concat "No. "(number-to-string (yon-elem post 'no)))
    'face 'yon-chan-post-number))
  (newline))

;; let's hard code this for now
(defun yon-browse-g (buffer)
  (url-retrieve "http://api.4chan.org/g/catalog.json"
                (lexical-let ((yon-buffer buffer))
                  (lambda (status)
                    (yon-render yon-buffer
                                  'yon-render-board
                                  (yon-get-and-parse-json))))))

;;;###autoload
(defun yon-chan ()
  "Fetch and display 4chan boards."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window
                        (generate-new-buffer "*yon-chan*"))
    (yon-chan-mode)
    (yon-browse-g (current-buffer))))


(provide 'yon-chan)
;;; yon-chan.el ends here
