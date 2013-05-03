#!/bin/sh -e

cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}
TRAVIS="true"
export TRAVIS
export ECUKES_EMACS

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

./run-tests.sh $TAGS
INTEGRATION=$?
./run-unit-tests.sh
UNIT=$?

exit $INTEGRATION && $UNIT
