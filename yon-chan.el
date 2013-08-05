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
(require 'dash)
(require 'yon-rendering)
(require 'yon-formatting)
(require 'yon-navigation)
(require 'yon-structures)

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

;;;
;;; Actions
;;;

(defun display-image-other-window (url name)
  (lexical-let ((name name))
    (url-retrieve
     (progn (message (concat "Retrieving " url)) url)
     (lambda (status)
       (rename-buffer name t)
       ;; Delete HTTP header
       (re-search-forward "\r?\n\r?\n")
       (delete-region (point-min) (point))
       (image-mode)
       (switch-to-buffer-other-window (buffer-name))))))

;;;
;;; Navigation
;;;



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
