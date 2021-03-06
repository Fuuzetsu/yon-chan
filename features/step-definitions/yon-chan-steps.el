;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(eval-when-compile (require 'cl))
(require 'yon-chan)

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(Then "^I should see exactly\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the current buffer includes some text."
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected '%s' and got '%s'."))
          (assert (s-equals? expected actual) nil message expected actual))))

(Then "^the cursor should be on line \"\\(.+\\)\"$"
      (lambda (l)
        (assert (= (line-number-at-pos) (string-to-number l)) nil
                "Expected to be on line '%s' but are on line '%d'"
                l (line-number-at-pos))))

(When "^I render \"\\(.+\\)\" as \"\\(.+\\)\"$"
      (lambda (stub board)
        (insert-file-contents (concat "features/stubs/" stub))
        (yon-chan-mode)
        (set (make-local-variable 'yon-current-board) board)
        (yon-render (current-buffer)
                    'yon-render-thread
                    (yon-build-thread (yon-parse-json (buffer-string))
                                      (current-buffer)))))
