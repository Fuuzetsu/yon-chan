#!/usr/bin/env bash
EMACS=${ECUKES_EMACS:-emacs}

if [[ "$TRAVIS" == "true" ]]; then
    $EMACS -batch -l ert -l elpa/mocker*/mocker.el -l yon-chan.el -l yon-chan-tests.el -f ert-run-tests-batch-and-exit
else
    OUTPUT=$("$EMACS" -batch -l ert -l elpa/mocker*/mocker.el -l yon-chan.el -l yon-chan-tests.el -f ert-run-tests-batch-and-exit 2>&1)
    STATUS=$?
    if [ $STATUS -ne 0 ]; then
        echo "${OUTPUT}"
        exit $EXITC
    fi
    echo ert tests passing.
fi
