;;; yon-chan.el --- A 4chan browser for Emacs

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

;; This mode uses the 4chan API to provide a board and thread browser
;; from within Emacs

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Yon-chan mode
(defgroup yon-chan nil
  "Group used to store various yon-chan properties.")

(defcustom yon-chan-mode-default-board nil
  "The default board to visit when starting yon-chan"
  :type 'string
  :group 'yon-chan)

(define-derived-mode yon-chan-mode
  special-mode "Yon-chan"
  "4chan browser."
  (define-key yon-chan-mode-map (kbd "n") 'yon-jump-post-forward)
  (define-key yon-chan-mode-map (kbd "p") 'yon-jump-post-backward)
  (define-key yon-chan-mode-map (kbd "P") 'yon-jump-post-first)
  (define-key yon-chan-mode-map (kbd "N") 'yon-jump-post-last)
  (define-key yon-chan-mode-map (kbd "r") 'yon-refresh-buffer))

(defvar yon-chan-mode-map (make-sparse-keymap)
  "yon-chan-mode keymap")

;;; Faces

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
    (+ match (length close))))

(defun yon-get-section (start end)
  (save-excursion
    (buffer-substring start end)))


;; Interactive for now for testing
(defun yon-possibly-colorify-line-by-tags
  (opregex endregex colourface &optional newline)
  "Applies colourface to anything between opregex and endregex.
If newline is non-nil, newlines in the matching text will be removed."
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

;;; Posts

;; The atoms of 4chan.

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


(defun yon-build-thread (response buffer)
  (with-current-buffer buffer
    (set (make-local-variable 'yon-buffer-posts)
         (mapcar 'yon-build-post (yon-elem response 'posts)))))

(defun flatten(x)
  (cond ((null x) nil)
    ((listp x) (append (flatten (car x)) (flatten (cdr x))))
    (t (list x))))

(defun yon-build-catalog (response buffer)
  (with-current-buffer buffer
    (let ((pages (mapcar 'yon-build-page response)))
      (set (make-local-variable 'yon-buffer-posts) (flatten pages))
      pages)))

(defun yon-build-page (response)
  (mapcar 'yon-build-post (yon-elem response 'threads)))

;;; Rendering

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
                             (yon-format-post-image post)))))
  ;; Some header items could be blank (such as the image filename), so
  ;; remove all nil values.
  (json-join  (remq nil items) " - "))

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
      (concat (yon-post-filename post) (yon-post-extension post))
    ""))

(defun yon-format-post-number (post)
  "Returns the post number. Clickable if it's a thread OP."
  (lexical-let ((op (equal 0 (yon-post-replyto post)))
                (kmap (make-sparse-keymap))
                (board (with-current-buffer (buffer-name)
                         (when (boundp 'yon-current-board)
                             yon-current-board)))
                (post-number (number-to-string (yon-post-number post))))
    (define-key kmap (kbd "<return>")
      (lambda ()
        (interactive)
        (yon-browse-thread
         (switch-to-buffer-other-window
          (generate-new-buffer
           (concat "*yon-chan-/" board "/-" post-number)))
         board post-number)))
    (define-key kmap (kbd "a")
      (lambda ()
        (interactive)
        (yon-browse-thread
         (with-current-buffer
             (rename-buffer (concat "*yon-chan-/" board "/-" post-number))
           (current-buffer))
         board post-number)))
    (if (and op board)
        (with-temp-buffer
          (insert-text-button post-number
                              'face 'yon-face-post-number-link
                              'keymap kmap)
          (buffer-string))
      (propertize post-number 'face 'yon-face-post-number))))


(defun yon-jump-posts (amount)
  "Jumps `amount' of posts. Can be negative."
  (let* ((posts (with-current-buffer (current-buffer) yon-buffer-posts))

         (sorted-posts (sort yon-buffer-posts
                             (lambda (x y)
                               ;; This currently is just a safety measure,
                               ;; they should be presorted for now.
                               "Sorts posts based on their render position."
                               (< (yon-post-renderpos x)
                                  (yon-post-renderpos y)))))
         (closest-post-index
          (let ((closest (yon-post-renderpos (car posts)))
                (index 0))
            (dotimes (idx (length sorted-posts)) ;; it's like I'm really writing C!
              (let ((rp (yon-post-renderpos (nth idx sorted-posts))))
                (when (> (abs (- (point) closest)) (abs (- (point) rp)))
                  (setq closest rp)
                  (setq index idx))))
            index))
         (new-idx (+ closest-post-index amount)))
    (if (< new-idx 0)
        (yon-jump-to-local-post (yon-post-number (nth 0 sorted-posts)))
      (if (>= new-idx (length sorted-posts))
          (yon-jump-to-local-post (yon-post-number (car (last sorted-posts))))
        (yon-jump-to-local-post (yon-post-number (nth new-idx sorted-posts)))))))

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


(defun yon-browse-board-catalog (buffer board)
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
  "Refreshes arbitrary buffer"
  (interactive)
  (with-current-buffer (if buffer buffer (current-buffer))
    (funcall yon-refresh)))

(defun yon-browse-thread (buffer board thread-number)
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
  "Shows the specified board's catalog."
  (interactive "MEnter the board you wish to visit: ")
  (let ((clean-board (if (string-match "/.+?/" board)
                         (substring board 1 -1)
                       board)))
    (with-current-buffer (switch-to-buffer-other-window
                          (generate-new-buffer (concat "*yon-chan-/"
                                                       clean-board "/*")))
      (yon-chan-mode)
      (yon-browse-board-catalog (current-buffer) clean-board))))

;;;###autoload
(defun yon-chan ()
  "Fetch and display 4chan boards."
  (interactive)
  (if yon-chan-mode-default-board
      (with-current-buffer (switch-to-buffer-other-window
                            (generate-new-buffer
                             (concat "*yon-chan-/"
                                     yon-chan-mode-default-board "/*")))
        (yon-chan-mode)
        (yon-browse-board-catalog (current-buffer) yon-chan-mode-default-board))
    (call-interactively 'yon-chan-browse-board)))


(provide 'yon-chan)
;;; yon-chan.el ends here
