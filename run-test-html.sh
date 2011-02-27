#!/bin/sh
#
# http://www.chromeplugins.org/tips-tricks/chrome-command-line-switches/
# http://peter.sh/experiments/chromium-command-line-switches/
# --single-process 
# --host-rules=\"MAP *.google.com 127.0.0.1\"
# /chrome/trunk/src/chrome/common/chrome_switches.cc
#
export GOOGLE_CHROME_OPTIONS="--disable-translate --no-default-browser-check --always-enable-dev-tools"
google-chrome $GOOGLE_CHROME_OPTIONS --user-data-dir=/tmp/DebugProfile test/client/maori-test-runner.html
