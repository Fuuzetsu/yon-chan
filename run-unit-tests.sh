#!/usr/bin/env bash
EMACS=${ECUKES_EMACS:-emacs}
PRINT_ANYWAY="true"

if [[ "$TRAVIS" == "true" || "$PRINT_ANYWAY" == "true" ]]; then
    $EMACS -batch -l ert -l elpa/dash*/dash.el -l elpa/mocker*/mocker.el -L . -l yon-chan-tests.el -f ert-run-tests-batch-and-exit
else
    OUTPUT=$("$EMACS" -batch -l ert -l elpa/dash*/dash.el -l elpa/mocker*/mocker.el -L . -l yon-chan-tests.el -f ert-run-tests-batch-and-exit 2>&1)
    STATUS=$?
    if [ $STATUS -ne 0 ]; then
        echo "${OUTPUT}"
        exit $EXITC
    fi
    echo ert tests passing.
fi
