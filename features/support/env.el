;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq yon-chan-root-path project-directory)
  (setq yon-chan-util-path (expand-file-name "util" yon-chan-root-path)))

(add-to-list 'load-path yon-chan-root-path)
(add-to-list 'load-path (expand-file-name "espuds" yon-chan-util-path))
(add-to-list 'load-path (expand-file-name "ert" yon-chan-util-path))

(require 'yon-chan)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 )

(Before
 (erase-buffer)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
