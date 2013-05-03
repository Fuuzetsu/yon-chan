#!/bin/sh -e
ECUKES=$(find elpa/ecukes-*/ecukes | tail -1)

carton exec "$ECUKES" "$@"
if [[ "$TRAVIS" == "true" ]]; then
    exit $?
fi

if [ $? -eq 0 ]; then
    echo Ecukes tests passing.
fi
