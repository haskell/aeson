#!/bin/bash

if hash hlint 2>/dev/null; then
    hlint "$@"
else
    ./hlint.sh "$@"
fi
