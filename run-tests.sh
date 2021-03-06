#!/usr/bin/env bash
ECUKES=$(find .cask/*/elpa/ecukes-*/bin/ecukes | tail -1)

cask exec "$ECUKES" "$@"
RESULT=$?
if [[ "$TRAVIS" == "true" ]]; then
    exit $RESULT
fi

if [[ "$RESULT" == "0" ]]; then
    echo Ecukes tests passing.
fi

exit $RESULT
