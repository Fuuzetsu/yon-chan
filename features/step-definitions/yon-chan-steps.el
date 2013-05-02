;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(eval-when-compile (require 'cl))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(When "^I go to the \\(beginning\\|end\\) of the \\(line\\|buffer\\)$"
      (lambda (pos word)
        (if (string-equal "front" pos) (backward-word)
          (if (string-equal "line" word)
              (move-beginning-of-line)
            (beginning-of-buffer))
          (if (string-equal "line" word)
              (move-end-of-line)
            (end-of-buffer)))))



(Then "^I should see exactly\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the current buffer includes some text."
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected '%s' and got '%s'."))
          (assert (s-equals? expected actual) nil message expected actual))))