;; yon-structures.el --- Structure functions for yon-chan

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

;; This file contains various structures used throughout yon-chan

;;; Code:

(cl-defstruct yon-post
  subject
  author
  timestamp
  number
  comment
  filename
  replyto
  sticky
  closed
  time
  trip
  id
  capcode
  country
  country-name
  email
  extension
  file-size
  md5
  image-width
  image-height
  thumbnail-width
  thumbnail-height
  file-deleted
  spoiler
  custom-spoiler
  omitted-posts
  omitted-images
  replies
  images
  bumplimit
  imagelimit
  new-filename
  renderpos)

(defun yon-build-post (response)
  "Builds a post object from deserialized JSON response."
  (make-yon-post
   :subject          (yon-elem response 'sub "No subject")
   :author           (yon-elem response 'name "Anonymous")
   :timestamp        (yon-elem response 'now)
   :number           (yon-elem response 'no)
   :comment          (yon-elem response 'com "")
   :filename         (yon-elem response 'filename)
   :replyto          (yon-elem response 'resto) ;; 0 is OP
   :sticky           (yon-elem response 'sticky)
   :closed           (yon-elem response 'closed)
   :new-filename     (yon-elem response 'tim)
   :time             (yon-elem response 'time)
   :trip             (yon-elem response 'trip)
   :id               (yon-elem response 'id)
   :capcode          (yon-elem response 'capcode)
   :country          (yon-elem response 'country)
   :country-name     (yon-elem response 'country_name)
   :email            (yon-elem response 'email)
   :extension        (yon-elem response 'ext)
   :file-size        (yon-elem response 'fsize)
   :md5              (yon-elem response 'md5)
   :image-width      (yon-elem response 'w)
   :image-height     (yon-elem response 'h)
   :thumbnail-width  (yon-elem response 'tn_w)
   :thumbnail-height (yon-elem response 'tn_h)
   :file-deleted     (yon-elem response 'filedeleted)
   :spoiler          (yon-elem response 'spoiler)
   :custom-spoiler   (yon-elem response 'custom_spoiler)
   :omitted-posts    (yon-elem response 'omitted_posts)
   :omitted-images   (yon-elem response 'omitted_images)
   :replies          (yon-elem response 'replies)
   :images           (yon-elem response 'images)
   :bumplimit        (yon-elem response 'bumplimit)
   :imagelimit       (yon-elem response 'imagelimit)
   :renderpos        '()))


(provide 'yon-structures)
;;; yon-structures.el ends here
