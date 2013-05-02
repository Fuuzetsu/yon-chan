;;; yon-chan-test.el --- yon-chan's tests for non-interactive functions

(require 'ert)
(require 'yon-chan)

(ert-deftest closed-test-tag-finding ()
  "Tests that quote tags are found properly in text"
  (let ((simple-text "<span class=\"quote\">>Hello world!</span>")
        (multi-text "xxx<span class=\"quote\">>Multi
test</span>")
        (long-multi-text "xxxxxxxxxx<span class=\"quote\">>Multieuoa
anoethu
test</span>"))
    (should (eql (yon-get-closing-point simple-text "</span>") 40))
    (should (eql (yon-get-closing-point multi-text "</span>") 41))
    (should (eql (yon-get-closing-point long-multi-text "</span>") 60))))


(ert-deftest open-test-tag-finding ()
  "Tests that we can identify opening quote tag at the correct position"
  (let ((simple-text "<span class=\"quote\">>Hello world!</span>")
        (multi-text "xxx<span class=\"quote\">>Multi
test</span>")
        (long-multi-text "xxxxxxxxxx<span class=\"quote\">>Multieuoa
anoethu
test</span>"))
    (should (eql (string-match "<span class=\"quote\">" simple-text) 0))
    (should (eql (string-match "<span class=\"quote\">" multi-text) 3))
    (should (eql (string-match "<span class=\"quote\">" long-multi-text) 10))))

;; Utils

(deftest test-yon-elem ()
  "Test deserialized JSON value fetching"
  (should (string= (yon-elem '((foo . "bar")) 'foo) "bar"))
  (should (string= (yon-elem '() 'foo) nil))
  (should (string= (yon-elem '() 'foo "bar") "bar")))

;;; Posts

(deftest yon-build-post ()
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

(defvar response '((sub "Hello world")))

(defvar expected (make-yon-post :subject "Hello world"))
(car (yon-elem response 'sub))

(equalp (yon-build-post response) expected)
(yon-build-post response)
[cl-struct-yon-post ("Hello world") ("Anonymous") ("right now") (9001) ("This is a test")]
expected
[cl-struct-yon-post "Hello world" "Anonymous" "right now" 9001 "This is a test"]


(provide 'yon-chan-tests)
