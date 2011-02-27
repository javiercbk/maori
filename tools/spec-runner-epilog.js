// -*- coding: utf-8; -*-



/**
 * ShellReporter
 * @return {jasmine.Reporter} x.
 * @constructor
 */
function ShellReporter() {

  const ANSI_GREEN = '\033[32m';
  const ANSI_RED = '\033[31m';
  const ANSI_YELLOW = '\033[33m';
  const ANSI_NONE = '\033[0m';

  // la asignacion al prototipo es al pedo
  return {}.prototipe = {
    // new jasmine.Reporter();
    /**
     * @param {String} string some message.
     */
    log: function(string) {
      print(string);
    },
    /**
     * @param {jasmine.Runner} runner some meaningful description.
     */
    reportRunnerResults: function(runner) {
      //print('');
    },
    /**
     * @param {jasmine.Runner} runner some meaningful description.
     */
    reportRunnerStarting: function(runner) {
      //print('');
    },
    /**
     * @param {jasmine.Spec} spec some meaningful description.
     */
    reportSpecResults: function(spec) {
      var results = spec.results();
      if (results.passed()) {
        print('success: ' + spec.description);
      } else {
        print('failure: ' + spec.description +
              ' [\n' + failureMessageFor(results) + '\n]');
      }
    },
    /**
     * @param {jasmine.Spec} spec some meaningful description.
     */
    reportSpecStarting: function(spec) {
      print('test: ' + spec.description);
    },
    /**
     * @param {jasmine.Suite} suite some meaningful description.
     */
    reportSuiteResults: function(suite) {
      /*print('reportSuiteResults');
      while (suite) {
        print('>>>' + suite.description);
        suite = suite.parentSuite;
      }*/
    }
  };

  /*
  ejemplo de "hoisting" creo
  const ANSI_GREEN = '\033[32m';
  const ANSI_RED = '\033[31m';
  const ANSI_YELLOW = '\033[33m';
  const ANSI_NONE = '\033[0m';
  */

  function failureMessageFor(results) {
    var message = '';

    var resultItems = results.getItems();
    for (var i = 0; i < resultItems.length; ++i) {
      var result = resultItems[i];
      if (result.passed && !result.passed()) {
        message += result.message;
      }
    }
    return message;
  }
}

// we don't have nothing to update.
jasmine.getEnv().updateInterval = 0;
var a = ShellReporter();
jasmine.getEnv().addReporter(a);
jasmine.getEnv().execute();

//var b = ShellReporter();
//print(a === b);
//print(a.prototipe === b.prototipe);
