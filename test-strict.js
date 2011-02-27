try {
  eval('"use strict"; eval(\'"\\010"\');');
  print('Fail:\tExpected SyntaxError when using octal escape sequences');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; eval("010");');
  print('Fail:\tExpected SyntaxError when using octal literals');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; __i_dont_exist = 1;');
  print('Fail:\tExpected ReferenceError when using an undeclared variable');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; eval = 1');
  print('Fail:\tExpected SyntaxError when assigning eval');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; arguments = 1');
  print('Fail:\tExpected SyntaxError when assigning arguments');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; eval++');
  print('Fail:\tExpected SyntaxError when incrementing eval');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; arguments++');
  print('Fail:\tExpected SyntaxError when incrementing arguments');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; arguments.caller');
  print('Fail:\tExpected SyntaxError when accessing caller' +
        'property of arguments');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; arguments.callee');
  print('Fail:\tExpected SyntaxError when accessing callee' +
        'property of arguments');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; ({ x: 1, x: 1 });');
  print('Expected SyntaxError when using repeated properties in an object');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; ({ set x(eval){ } });');
  print('Fail:\tExpected SyntaxError when using eval as an argument');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; ({ set x(arguments){ } });');
  print('Fail:\tExpected SyntaxError when using arguments as an argument');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; eval("var x"); x;');
  print('Fail:\tExpected ReferenceError when accessing a variable ' +
        'defined in an eval');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; var x; delete x;');
  print('Fail:\tExpected SyntaxError when applying delete to an ' +
        'unqualified name');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; delete (function(){}).length;');
  print('Fail:\tExpected TypeError when deleting length property ' +
        'of a function');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function f() { f = 123; })();');
  print('Fail:\tExpected TypeError when assigning the functions name');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; Object.defineProperty({ }, "x", ' +
       '{ writable: false }).x = 1;');
  print('Fail:\tExpected TypeError when writing a read only property');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; ({ get x(){ } }).x = 1;');
  print('Fail:Expected TypeError when writing a property ' +
        'that only has a getter\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; var eval');
  print('Fail:\tExpected SyntaxError when defining eval as a variable');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; var arguments');
  print('Fail:\tExpected SyntaxError when defining arguments as a variable');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; with({}){ };');
  print('Fail:\tExpected SyntaxError when using a with statement');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; try { } catch (eval) { };');
  print('Fail:\tExpected SyntaxError when redefining eval inside a catch');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; try { } catch (arguments) { };');
  print('Fail:Expected SyntaxError when redefining arguments inside a catch\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; function f(eval) { };');
  print('Fail:\tExpected SyntaxError when redefining eval as an argument');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; function f(arguments) { };');
  print('Fail:\tExpected SyntaxError when redefining arguments as an argument');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; function f(x, x) { };');
  print('Fail:\tExpected SyntaxError when using duplicate argument name');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function(){}).caller;');
  print('Fail:\tExpected TypeError when accessing caller property ' +
        'of a function');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function(){}).arguments;');
  print('Fail:\tExpected TypeError when accessing arguments proerty ' +
        'of a function');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; function eval(){ };');
  print('Fail:Expected SyntaxError when using eval as a function name\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; function arguments(){ };');
  print('Fail:Expected SyntaxError when using arguments as a function name\t');
} catch (e) {
  print('Ok:\t' + e);
}

//No tiran error pero en la página no se aclara que error deberían tirar
try {
  eval('"use strict"; (function(x){ x = 2; return arguments[0] === 1; })(1);');
  print('Fail:\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function(x){ arguments[0] = 2; return x === 1; })(1);');
  print('Fail:\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function(){ return this === undefined; })();');
  print('Fail:\t');
} catch (e) {
  print('Ok:\t' + e);
}

try {
  eval('"use strict"; (function(){ return this === undefined; }).call();');
  print('Fail:\t');
} catch (e) {
  print('Ok:\t' + e);
}

print('.....');
