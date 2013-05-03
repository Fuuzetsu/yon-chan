;;; yon-chan-test.el --- yon-chan's tests for non-interactive functions

(require 'ert)
(require 'yon-chan)
(require 'mocker)

(ert-deftest test-yon-get-closing-point ()
  "Tests that quote tags are found properly in text"
  (let ((simple-text "<span class=\"quote\">>Hello world!</span>")
        (multi-text "xxx<span class=\"quote\">>Multi
test</span>")
        (long-multi-text "xxxxxxxxxx<span class=\"quote\">>Multieuoa
anoethu
test</span>"))
    (should (equal (yon-get-closing-point simple-text "</span>") 40))
    (should (equal (yon-get-closing-point multi-text "</span>") 41))
    (should (equal (yon-get-closing-point long-multi-text "</span>") 60))))


(ert-deftest test-open-tag-finding ()
  "Tests that we can identify opening quote tag at the correct position"
  (let ((simple-text "<span class=\"quote\">>Hello world!</span>")
        (multi-text "xxx<span class=\"quote\">>Multi
test</span>")
        (long-multi-text "xxxxxxxxxx<span class=\"quote\">>Multieuoa
anoethu
test</span>"))
    (should (equal (string-match "<span class=\"quote\">" simple-text) 0))
    (should (equal (string-match "<span class=\"quote\">" multi-text) 3))
    (should (equal (string-match "<span class=\"quote\">" long-multi-text) 10))))

;; Utils

(ert-deftest test-yon-elem ()
  "Test deserialized JSON value fetching"
  (should (string= (yon-elem '((foo . "bar")) 'foo) "bar"))
  (should (string= (yon-elem '() 'foo) nil))
  (should (string= (yon-elem '() 'foo "bar") "bar")))

(ert-deftest test-yon-build-catalog ()
  "Test that a sample catalog page is parsed properly."
  (with-temp-buffer
    (insert-file-contents "features/stubs/small-catalog.json")
    (let* ((catalog (yon-build-catalog (yon-parse-json (buffer-string))))
           (expected (list (yon-build-post
                            '((images . 4)
                              (replies . 43)
                              (trip . "!!MJ4VbGu9hJd")
                              (imagelimit . 0)
                              (bumplimit . 0)
                              (resto . 0)
                              (fsize . 274881)
                              (md5 . "37f6aLmlX3BiAteLJzmXwA==")
                              (time . 1367611690.0)
                              (tim . 1367611690801.0)
                              (tn_h . 140)
                              (tn_w . 250)
                              (h . 1080)
                              (w . 1920)
                              (ext . ".jpg")
                              (filename . "DSC00969")
                              (com . "This is a light pole outside of my house. What do you think it&#039;s for?")
                              (name . "OP")
                              (now . "05/03/13(Fri)16:08")
                              (no . 33511354))
                            )

                           (yon-build-post
                            '((images . 41)
                              (replies . 113)
                              (trip . "!VIRGIN/FJM")
                              (imagelimit . 0)
                              (bumplimit . 0)
                              (resto . 0)
                              (fsize . 692993)
                              (md5 . "ugTvIjLyKIEXfzh8z1EBWw==")
                              (time . 1367608288.0)
                              (tim . 1367608288151.0)
                              (tn_h . 140)
                              (tn_w . 250)
                              (h . 768)
                              (w . 1366)
                              (ext . ".png")
                              (filename . "destap")
                              (com . "Refreshed page 8366 times, saw no desktop threads.  Posting a desktop thread using start_desktop_thread.sh")
                              (sub . "desktop thread")
                              (email . "neko")
                              (name . "25 and")
                              (now . "05/03/13(Fri)15:11")
                              (no . 33510342)))))
           (should (equalp catalog expected))))))

;;; Posts

(ert-deftest test-yon-build-post ()
  "Test building post objects from deserialized JSON."
  (let ((response '((sub . "Hello world")
                    (name . "Anonymous")
                    (now . "right now")
                    (no . 9001)
                    (com . "This is a test")))
        (expected (make-yon-post :subject "Hello world"
                                 :author "Anonymous"
                                 :timestamp "right now"
                                 :number 9001
                                 :comment "This is a test")))
    (should (equalp (yon-build-post response) expected))))

;;; Formatting

(ert-deftest test-yon-format-post ()
  "Test formatting post string."
  (let ((post "a fake post"))
    (mocker-let ((yon-format-post-header (p)
                                         ((:input (list post) :output "foo")))
                 (yon-format-post-comment (p)
                                          ((:input (list post) :output "bar"))))
      (should (string= (yon-format-post post) "foo\nbar")))))

(ert-deftest test-yon-format-post-header ()
  "Test formatting post header string."
  (let ((post "a fake post"))
    (mocker-let ((yon-format-post-subject (p)
                                          ((:input (list post) :output "subject")))
                 (yon-format-post-author (p)
                                          ((:input (list post) :output "author")))
                 (yon-format-post-timestamp (p)
                                          ((:input (list post) :output "the time")))
                 (yon-format-post-number (p)
                                         ((:input (list post) :output 1))))
      (should (string= (yon-format-post-header post)
                       "subject - author - the time - 1")))))

(ert-deftest test-yon-format-post-comment ()
  "Test formatting post comment string."
  (let ((post (make-yon-post :comment "hello world")))
    (mocker-let ((yon-process-post (comment)
                                   ((:input '("hello world") :output "hello world"))))
      (should (string= (yon-format-post-comment post) "hello world")))))

(ert-deftest test-yon-format-post-subject ()
  "Test formatting post subject string"
  (let ((post (make-yon-post :subject "hello")))
    (mocker-let ((propertize (text property value)
                             ((:input '("hello" face yon-face-post-subject)
                                      :output "hello")))
                 (yon-clean-html-string (html)
                                        ((:input '("hello") :output "hello"))))
      (should (string= (yon-format-post-subject post) "hello")))))

(ert-deftest test-yon-format-post-author ()
  "Test formatting post subject string"
  (let ((post (make-yon-post :author "Anonymous")))
    (mocker-let ((propertize (text property value)
                             ((:input '("Anonymous" face yon-face-post-author)
                                      :output "Anonymous")))
                 (yon-clean-html-string (html)
                                        ((:input '("Anonymous") :output "Anonymous"))))
      (should (string= (yon-format-post-author post) "Anonymous")))))

(ert-deftest test-yon-format-post-number ()
  "Test formatting post number string"
  (let ((post (make-yon-post :number 1)))
    (mocker-let ((propertize (text property value)
                             ((:input '("1" face yon-face-post-number)
                                      :output "1"))))
      (should (string= (yon-format-post-number post) "1")))))

(provide 'yon-chan-tests)
