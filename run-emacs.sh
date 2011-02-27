#!/bin/sh

. ./tools/setup-shell.sh

emacs --eval "(load \"$PRJ_ROOT/tools/setup-emacs.el\")" > /dev/null 2>&1 $@ &

echo "*"
echo "* Start coding in your emacs...."
echo "*"
