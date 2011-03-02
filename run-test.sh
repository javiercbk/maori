#!/bin/sh

. ./tools/setup-shell.sh
#prueba
chmod ugoa+rx $V8_SHELL

$V8_SHELL \
    $PRJ_ROOT/tools/spec-runner-prolog.js \
    $PRJ_ROOT/lib/jasmine-1.0.1/jasmine.js \
    $PRJ_ROOT/src/client/general.js \
    $PRJ_ROOT/src/client/helper.js \
    $PRJ_ROOT/src/client/model.js \
    $PRJ_ROOT/src/client/event.js \
    $PRJ_ROOT/src/client/draw.js \
    $PRJ_ROOT/test/client/maori-test.js \
    $PRJ_ROOT/test/client/strict-test.js \
    $PRJ_ROOT/tools/spec-runner-epilog.js 
