#!/bin/bash
CURDIR=$(dirname $0)
# setup EDIT command
cp $CURDIR/edit /usr/local/bin/
# setup EmacsLauncher
cp -R $CURDIR/EmacsLauncher.app /Applications/
# setup logout hook
sudo defaults write com.apple.loginwindow LogoutHook $CURDIR/killEmacsBeforeLogout.sh
# END
