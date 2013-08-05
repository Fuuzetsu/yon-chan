#!/usr/bin/env bash
ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)

carton exec "$ECUKES" "$@"
RESULT=$?
if [[ "$TRAVIS" == "true" ]]; then
    exit $RESULT
fi

if [[ "$RESULT" == "0" ]]; then
    echo Ecukes tests passing.
fi

exit $RESULT
