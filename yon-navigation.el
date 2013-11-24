;; yon-navigation.el --- Navigation functions for yon-chan

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

;; This file contains various navigation functions used throughout yon-chan

;;; Code:

(defun yon-current-buffer-board ()
  "Returns the board name for the current buffer."
  (with-current-buffer (buffer-name)
    (when (boundp 'yon-current-board)
      yon-current-board)))

(defun yon-browse-thread-other-window (post)
  "Opens thread from the current board in a new window."
  (lexical-let* ((board (yon-current-buffer-board))
                 (post-number (number-to-string (yon-post-number post)))
                 (buffer (switch-to-buffer-other-window
                          (generate-new-buffer
                           (concat "*yon-chan-/"
                                   board
                                   "/-"
                                   post-number)))))
    (yon-browse-thread buffer board post-number)))

(defun yon-display-image-other-window (url name)
  "Fetch image from URL and display it in a buffer called NAME."
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

(defun yon-jump-posts (amount)
  "Jump AMOUNT of posts. Can be negative."
  (let* ((posts (with-current-buffer (current-buffer)
                  yon-buffer-posts))
         ;; zip index with distance
         (ixed
          (-zip-with
                (lambda (n p)
                  (cons n (* amount
                             (- (yon-post-renderpos p) (point)))))
                (number-sequence 0 (length posts)) posts))
         ;; only go one direction
         (dfilter (lambda (x) (> (cdr x) 0)))
         ;; posts going our way
         (cands (-filter dfilter ixed))
         (ocands (if (> amount 0)
                     cands
                   (reverse cands)))
         (am (abs amount)))
    (if cands
        (if (>= am (length posts))
            (yon-jump-to-local-post
             (yon-post-number
              (nth (caar (last ocands))
                   posts)))
          (yon-jump-to-local-post
           (yon-post-number
            (nth (car (nth (- am 1) ocands))
                 posts))))
      nil)))

(defun yon-jump-to-local-post (number)
  "Jump to post NUMBER in the current buffer."
  (let ((render-place))
    (dolist (post (with-current-buffer (current-buffer) yon-buffer-posts))
      (when (equal number (yon-post-number post))
        (setq render-place (yon-post-renderpos post))))
    (if render-place
        (progn
          (goto-char render-place)
          (recenter-top-bottom 0)) ;; I'm not too sure about this
      (message "Could not find post %s to jump to." (number-to-string number)))))

(defun yon-jump-to-nth (n)
  "Jump to Nth post. Jump to the first post when N is
negative. Jump to the last post when N is greater than the number
of posts in the current buffer."
  (let ((sorted-posts (sort yon-buffer-posts
                            (lambda (x y)
                              ;; This currently is just a safety measure,
                              ;; they should be presorted for now.
                              "Sorts posts based on their render position."
                              (< (yon-post-renderpos x)
                                 (yon-post-renderpos y))))))
    (cond
     ((< n 0) (yon-jump-to-local-post (yon-post-number (nth 0 sorted-posts))))
     ((> n (- (length sorted-posts) 1))
           (yon-jump-to-local-post
            (yon-post-number (nth (- (length sorted-posts) 1) sorted-posts))))
     (t (yon-jump-to-local-post (yon-post-number (nth n sorted-posts)))))))

(defun yon-jump-post-forward (&optional number)
  "Jump forward N posts. N defaults to 1."
  (interactive "p")
  (yon-jump-posts number))

(defun yon-jump-post-backward (&optional number)
  "Jump backward N posts. N defaults to 1."
  (interactive "p")
  (yon-jump-posts (* -1 number)))

(defun yon-jump-post-first ()
  "Jump to the first post in the buffer."
  (interactive)
  (yon-jump-to-nth 0))

(defun yon-jump-post-last ()
  "Jump to the last post in the buffer."
  (interactive)
  (yon-jump-to-nth (- (length yon-buffer-posts) 1)))

(defun yon-browse-board-catalog (buffer board)
  "View the catalog for BOARD in the buffer BUFFER."
  (url-retrieve (concat "http://api.4chan.org/" board "/catalog.json")
                (lexical-let ((yon-buffer buffer)
                              (board-l board))
                  (with-current-buffer yon-buffer
                    (yon-chan-mode)
                    (set (make-local-variable 'yon-current-board) board)
                    (set (make-local-variable 'yon-refresh)
                         (lambda ()
                           "Function called when we want to refresh the catalog"
                           (yon-browse-board-catalog yon-buffer board-l)))
                    (lambda (status)
                      (yon-render yon-buffer
                                  'yon-render-catalog
                                  (yon-build-catalog (yon-get-and-parse-json)
                                                     yon-buffer)))))))

(defun yon-refresh-buffer (&optional buffer)
  "Refresh the posts in BUFFER. BUFFER defaults to the current
buffer."
  (interactive)
  (with-current-buffer (if buffer buffer (current-buffer))
    (funcall yon-refresh)))

(defun yon-browse-thread (buffer board thread-number)
  "View thread THREAD-NUMBER for BOARD in BUFFER."
  (url-retrieve
   (concat "http://api.4chan.org/" board "/res/" thread-number ".json")
   (lexical-let ((yon-buffer buffer)
                 (board-l board)
                 (thread-number-l thread-number))
     (with-current-buffer yon-buffer
       (yon-chan-mode)
       (set (make-local-variable 'yon-current-board) board)
       (set (make-local-variable 'yon-refresh)
            (lambda ()
              "Function called when we want to refresh the whole thread"
              (yon-browse-thread yon-buffer board-l thread-number-l)))
       (lambda (status)
         (yon-render yon-buffer
                     'yon-render-thread
                     (yon-build-thread (yon-get-and-parse-json) yon-buffer)))))))

(defun yon-chan-browse-board (board)
  "View the catalog for BOARD in a new window."
  (interactive (list (completing-read "Enter the board you wish to visit: " yon-chan-boards)))
  (let ((clean-board (if (string-match "/.+?/" board)
                         (substring board 1 -1)
                       board)))
    (with-current-buffer (switch-to-buffer-other-window
                          (generate-new-buffer (concat "*yon-chan-/"
                                                       clean-board "/*")))
      (yon-chan-mode)
      (yon-browse-board-catalog (current-buffer) clean-board))))


(provide 'yon-navigation)
;;; yon-navigation.el ends here
