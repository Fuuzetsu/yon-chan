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

(ert-deftest test-yon-apply-quote-links ()
  "Test that links to post and threads get formatted properly."
  (let ((orig "<a href=\"579850#p579850\" class=\"quotelink\">>>579850</a>
meanwhile, test
<a href=\"/g/res/33526840#p33526840\" class=\"quotelink\">>>>/g/3352
6840</a>")
        (expected ">>579850
meanwhile, test
>>>/g/33526840"))
    (should (string= (with-temp-buffer
                       (insert orig)
                       (with-current-buffer (buffer-name)
                         (set (make-local-variable 'yon-current-board) "q"))
                       (yon-apply-quotelinks)
                       (buffer-string))
                     expected))))

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

(ert-deftest test-yon-format-post-number-not-op ()
  "Test formatting post number string"
  (let ((post (make-yon-post :number 1)))
    (setq yon-current-board nil) ;; TODO set :replyto properly once merged
    (mocker-let ((propertize (text property value)
                             ((:input '("1" face yon-face-post-number)
                                      :output "1"))))
      (should (string= (yon-format-post-number post) "1")))))


;; TODO set :replyto properly once merged
;; I can't think of a way to properly unit test this…
(ert-deftest test-yon-format-post-number-op ()
  "Test formatting post number string"
  (let ((post (make-yon-post :number 1)))
    (with-current-buffer (current-buffer)
      (set (make-local-variable 'yon-current-board) "q"))
    (should (string= (yon-format-post-number post) "1"))))

(provide 'yon-chan-tests)
