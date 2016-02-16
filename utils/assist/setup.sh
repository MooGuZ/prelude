#!/bin/bash
CURDIR=$(dirname $0)
# setup EDIT command
cp $CURDIR/edit /usr/local/bin/
# setup EmacsLauncher
cp -R $CURDIR/EmacsLauncher.app /Applications/
# END
