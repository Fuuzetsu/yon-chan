#!/bin/sh -e

cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}
export ECUKES_EMACS

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

./run-tests.sh $TAGS
INTEGRATION=$?
$ECUKES_EMACS -batch -l ert -l elpa/mocker*/mocker.el -l yon-chan.el -l yon-chan-tests.el -f ert-run-tests-batch-and-exit
UNIT=$?

exit $INTEGRATION && $UNIT
