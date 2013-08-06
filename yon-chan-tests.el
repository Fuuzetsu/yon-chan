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
  "Test that a simple catalog page is parsed properly."
  (let ((result-buffer (generate-new-buffer "ert-test-yon-render-catalog")))
    (with-current-buffer result-buffer
      (set (make-local-variable 'yon-current-board) "g"))
    (with-temp-buffer
      (insert-file-contents "features/stubs/small-catalog.json")
      (let* ((catalog (yon-build-catalog (yon-parse-json (buffer-string)) result-buffer))
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
                                (no . 33511354)))
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
             (should (equalp catalog expected)))))))

;; This test's formatting is weird to ensure all whitespace is preserved
(ert-deftest test-yon-render-catalog ()
  "Test that a simple catalog page is rendered properly."
  (let ((result-buffer (generate-new-buffer "ert-test-yon-render-catalog")))
    (with-current-buffer result-buffer
      (set (make-local-variable 'yon-current-board) "g"))
    (with-temp-buffer
      (insert-file-contents "features/stubs/small-catalog.json")
      (let* ((catalog (yon-build-catalog (yon-parse-json (buffer-string))
                                         result-buffer)))
        (with-current-buffer result-buffer
          (yon-render-catalog catalog)
          (should (string= (buffer-string)
                           (concat "No subject - Anonymous - 05/03/13(Fri)10:16 - 33505434 - tumblr_mkkn4lO8TQ1s6c71uo1_250.jpg
Anyone still using their Raspberry Pi or did you get bored? " "

What have you guys done with yours?

No subject - Anonymous - 05/03/13(Fri)06:23 - 33502527 - 144p.jpg
Why haven't you joined the 144p master race yet /g/?

"))))))))

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


(ert-deftest test-yon-apply-quote-links-single ()
  "Test that links to post and threads get formatted properly."
  (let ((orig "hello <a href=\"579850#p579850\" class=\"quotelink\">>>579850</a>")
        (expected "hello >>579850"))
    (set (make-local-variable 'yon-current-board) "q")
    (should (string= (yon-apply-quotelinks orig)
                     expected))))

(ert-deftest test-yon-apply-quote-links-multi ()
  "Test that links to post and threads get formatted properly."
  (let ((orig (concat "hello "
                      "<a href=\"579850#p579850\" "
                      "class=\"quotelink\">>>579850</a> "
                      "<a href=\"/g/res/33526840#p33526840\" "
                      "class=\"quotelink\">>>>/g/33526840</a> "
                      "world"))
        (expected "hello >>579850 >>>/g/33526840 world"))
    (set (make-local-variable 'yon-current-board) "q")
    (should (string= (yon-apply-quotelinks orig)
                     expected))))


(ert-deftest test-yon-extract-quote-link ()
  "Tests that given a string, we can splice out the relevant parts"
  (let ((orig "<a href=\"579850#p579850\" class=\"quotelink\">>>579850</a>")
        (expected '("579850#p579850" ">>579850")))
    (should (equal (yon-extract-quote-link orig) expected))))


(ert-deftest test-yon-apply-deadlinks ()
  "Test that deadlinks are properly caught and substituted"
  (let ((input-text "top empty
Midline deadlink <span class=\"deadlink\">>>>/g/123456</span> endline
more text
midbroken <span class=\"deadlink\">>>>/g/6425
7533</span> deadlink
end test")
        (result-text "top empty
Midline deadlink >>>/g/123456 endline
more text
midbroken >>>/g/64257533 deadlink
end test"))
    (set (make-local-variable 'yon-current-board) "g")
    (should (string= result-text (yon-apply-deadlinks input-text)))))

(ert-deftest test-yon-apply-greentext ()
  "Test that greentext is properly caught and substituted"
  (let ((input-text "top empty
<span class=\"quote\">>please go OP</span>
<span class=\"quote\">>staying</span>
Long lines are long.
<span class=\"quote\">>test test test</span>
more text
even more text
<span class=\"quote\">>Multi
 track
 greentext!</span>
<span class=\"quote\">></span>
end test")
        (result-text "top empty
>please go OP
>staying
Long lines are long.
>test test test
more text
even more text
>Multi track greentext!
>
end test"))
    (set (make-local-variable 'yon-current-board) "g")
    (should (string= result-text (yon-apply-greentext input-text)))))

;;; Formatting

(ert-deftest test-yon-format-post ()
  "Test formatting post string."
  (let ((post "a fake post"))
    (mocker-let ((yon-format-post-header (p)
                                         ((:input (list post) :output "foo")))
                 (yon-format-post-comment (p)
                                          ((:input (list post) :output "bar"))))
      (should (string= (yon-format-post post) "foo\nbar\n")))))

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
                                         ((:input (list post) :output "1")))
                 (yon-format-post-image (p)
                                        ((:input (list post) :output "foo.png"))))
      (should (string= (yon-format-post-header post)
                       "subject - author - the time - 1 - foo.png")))))

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
  (let ((post (make-yon-post :number 1
                             :replyto 1337)))
    (mocker-let ((propertize (text property value)
                             ((:input '("1" face yon-face-post-number)
                                      :output "1"))))
      (should (string= (yon-format-post-number post) "1")))))


;; I can't think of a way to properly unit test this…
(ert-deftest test-yon-format-post-number-op ()
  "Test formatting post number string"
  (let ((post (make-yon-post :number 1
                             :replyto 0)))
    (with-current-buffer (current-buffer)
      (set (make-local-variable 'yon-current-board) "q"))
    (should (string= (yon-format-post-number post) "1"))))

(provide 'yon-chan-tests)
