#!/usr/bin/env bash

USERSCRIPTS="$HOME/.local/share/qutebrowser/userscripts"

pack build qutescript-debug
pack build qutescript-github

rm -rvf "$USERSCRIPTS/qutescript-debug*"
rm -rvf "$USERSCRIPTS/qutescript-github*"

cp -rv examples/debug/build/exec/* "$USERSCRIPTS/"
cp -rv examples/github/build/exec/* "$USERSCRIPTS/"
