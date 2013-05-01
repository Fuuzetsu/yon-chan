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

(provide 'yon-chan-tests)
