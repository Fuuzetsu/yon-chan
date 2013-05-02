#!/bin/sh
emacs -batch -l ert -l yon-chan.el -l yon-chan-tests.el -f ert-run-tests-batch-and-exit
