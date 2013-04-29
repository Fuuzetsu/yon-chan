;;; 4chan.el --- A 4chan browser for Emacs

;; Copyright (C) 2013  David Thompson

;; Author: David Thompson <dave@labrys>
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

(define-derived-mode yon-chan-mode
  special-mode "4chan"
  "4chan browser.")

;; (defun yon-mode-keys ()
;;   "Set local key defs for yon-mode"
;;   (define-key yon-mode-map "q" 'quit-window)

(defvar yon-api-url "http://api.4chan.org/")

(defun yon-elem (alst key &optional default)
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

(defun yon-render (buffer proc json)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (funcall proc json)
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
  (insert (yon-elem post 'sub "No subject"))
  (insert " - ")
  (insert (yon-elem post 'name "No name"))
  (insert " - ")
  (insert (yon-elem post 'now))
  (insert " - No. ")
  (insert (number-to-string (yon-elem post 'no)))
  (newline)
  (insert (yon-elem post 'com ""))
  (newline)
  (newline))

(defun yon-render-op-post (post)
  (insert (yon-elem post 'sub "No subject"))
  (insert " - ")
  (insert (yon-elem post 'name "No name"))
  (insert " - ")
  (insert (yon-elem post 'now))
  (insert " - No. ")
  (insert (number-to-string (yon-elem post 'no)))
  (newline)
  (auto-fill-mode t)
  (insert (yon-elem post 'com ""))
  (auto-fill-mode nil)
  (newline)
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
  "Load 4chan."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (generate-new-buffer "*4chan*"))
    (yon-chan-mode)
    (yon-browse-g (current-buffer))))

(provide 'yon-chan)
;;; 4chan.el ends here
