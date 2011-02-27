#!/bin/sh

export PRJ_ROOT=$PWD
export V8_SHELL=$PRJ_ROOT/d8
#export PYTHONPATH=$PYTHONPATH:$PRJ_ROOT/tools/closure_linter

#Check para ver si el gjslint esta intalado
#type -P gjslint &>/dev/null &2>&1 || { echo "gjslint is required and will be installed" >&2; sudo apt-get install python-setuptools; sudo easy_install http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz; }
