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

(defun yon-jump-posts (amount)
  "Jumps `amount' of posts. Can be negative."
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
  "Jumps to nth post. Negative n gives first post too high n gives last post."
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
  "Jump backward one post. Jump forward `n' posts if given an argument."
  (interactive "p")
  (yon-jump-posts number))

(defun yon-jump-post-backward (&optional number)
  "Jump backward one post. Jump backward `n' posts if given an argument."
  (interactive "p")
  (yon-jump-posts (* -1 number)))

(defun yon-jump-post-first ()
  "Jump to first post on the page."
  (interactive)
  (yon-jump-to-nth 0))

(defun yon-jump-post-last ()
  "Jump to last post on the page."
  (interactive)
  (yon-jump-to-nth (- (length yon-buffer-posts) 1)))


(provide 'yon-navigation)
;;; yon-navigation.el ends here
