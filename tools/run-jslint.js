// -*- coding: utf-8; -*-


/**
 * Runs JSLINT on a file
 *
 * @param {String} fname file name.
 */
function RunJSLINT(fname) {
  'use strict';
  //
  var options = {
    white: true,
    onevar: true,
    undef: false,
    newcap: true,
    nomen: true,
    regexp: true,
    plusplus: true,
    bitwise: true,
    strict: true,
    es5: true,
    devel: true,
    maxlen: 132
  }, i, e, source;
  //
  source = read(fname);
  if (JSLINT(source, options)) {
    print('JSLINT says: everything is OK...');
  } else {
    //for (i in JSLINT.errors) {
    for (i = 0; i < JSLINT.errors.length; i = i + 1) {
      e = JSLINT.errors[i];
      if (e) {
        print(fname + ':' + e.line + ':' + e.character + ': ' + e.reason);
      }
    }
  }
}
