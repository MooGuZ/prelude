#!/bin/bash
ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
if [ $(ps -A | grep -c -e "Emacs\.app.*daemon") -gt 0 ]; then
	$ec -e "(kill-emacs)" > /dev/null 2>&1
fi

