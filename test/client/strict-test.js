'use strict';

xdescribe('Al usar strict mode', function() {
  it('Debe haber un error al usar una secuencia de escape octal', function() {
    expect(function() {
      eval('eval(\'"\\010"\')');
    }).toThrow('octal literals and octal escape sequences are deprecated');
  });

  it('Debe haber un error al usar un octal literal', function() {
    expect(function() {
      eval('eval("010")');
    }).toThrow('octal literals and octal escape sequences are deprecated');
  });

  it('Debe haber un error al usar una variable no definida', function() {
    expect(function() {
      eval('__i_dont_exist = 1');
    }).toThrow('assignment to undeclared variable __i_dont_exist');
  });

  it('Debe haber un error al asignar algo a eval', function() {
    expect(function() {
      eval('eval = 1');
    }).toThrow('assignment to eval is deprecated');
  });

  it('Debe haber un error al asignar algo a arguments', function() {
    expect(function() {
      eval('arguments = 1');
    }).toThrow('assignment to arguments is deprecated');
  });

  it('Debe haber un error al modificar a eval', function() {
    expect(function() {
      eval('eval++');
    }).toThrow('assignment to eval is deprecated');
  });

  it('Debe haber un error al modificar arguments', function() {
    expect(function() {
      eval('arguments++');
    }).toThrow('assignment to arguments is deprecated');
  });

  it('Debe haber un error al acceder a la propiedad' +
     'caller de arguments', function() {
       expect(function() {
         eval('arguments.caller');
       }).toThrow('\'caller\', \'callee\', and \'arguments\' properties may ' +
                  'not be accessed on strict mode functions or the arguments ' +
                  'objects for calls to them');
     });

  it('Debe haber un error al acceder a la propiedad' +
     'callee de arguments', function() {
       expect(function() {
         eval('arguments.callee');
       }).toThrow('\'caller\', \'callee\', and \'arguments\' properties may ' +
                  'not be accessed on strict mode functions or the arguments ' +
                  'objects for calls to them');
     });

  it('Debe haber un error al tener dos propiedades ' +
     'con el mismo nombre', function() {
       expect(function() {
         eval('({ x: 1, x: 1 });');
       }).toThrow('property name x appears more than once in object literal');
     });

  it('Debe haber un error al redefinir eval', function() {
    expect(function() {
      eval('({ set x(eval){ } });');
    }).toThrow('redefining eval is deprecated');
  });

  it('Debe haber un error al redefinir arguments', function() {
    expect(function() {
      eval('({ set x(arguments){ } });');
    }).toThrow('redefining arguments is deprecated');
  });

  it('Debe haber un error al usar una variable definida en eval', function() {
    expect(function() {
      eval('var x');
      x;
    }).toThrow('x is not defined');
  });

  it('Debe haber un error al borrar una variable global', function() {
    expect(function() {
      eval('var x; delete x;');
    }).toThrow('applying the \'delete\' operator to ' +
               'an unqualified name is deprecated');
  });

  it('Debe haber un error al borrar la propedad length', function() {
    expect(function() {
      delete (function() {}).length;
    }).toThrow('property \"use strict\";function () {}.length is ' +
               'non-configurable and can\'t be deleted');
  });

  it('Debe haber un error al sobreescribir el ' +
     'nombre de la funcion', function() {
       expect(function() {
         (function f() { f = 123; })();
       }).toThrow('\"use strict\";f is read-only');
     });

  it('Debe haber un error al acceder a una propiedad read only', function() {
    expect(function() {
      Object.defineProperty({ }, 'x', { writable: false }).x = 1;
    }).toThrow('\"use strict\";Object.defineProperty({}, \"x\", ' +
               '{writable: false}).x is read-only');
  });

  it('Debe haber un error al setear una propiedad ' +
     'que solo tiene un getter', function() {
       expect(function() {
         ({ get x() { } }).x = 1;
       }).toThrow('setting a property that has only a getter');
     });

  it('Debe haber un error al querer definir eval', function() {
    expect(function() {
      eval('var eval;');
    }).toThrow('redefining eval is deprecated');
  });

  it('Debe haber un error al querer definir arguments', function() {
    expect(function() {
      eval('var arguments;');
    }).toThrow('redefining arguments is deprecated');
  });

  it('Debe haber un error al usar el comando with', function() {
    expect(function() {
      eval('with({}){ };');
    }).toThrow('strict mode code may not contain \'with\' statements');
  });

  it('Debe haber un error al redefinir eval en un catch', function() {
    expect(function() {
      eval('try { } catch (eval) { };');
    }).toThrow('redefining eval is deprecated');
  });

  it('Debe haber un error al redefinir arguments en un catch  ', function() {
    expect(function() {
      eval('try { } catch (arguments) { };');
    }).toThrow('redefining arguments is deprecated');
  });

  it('Debe haber un error al redefinir eval como argumento de una ' +
     'función', function() {
       expect(function() {
         eval('function f(eval) { };');
       }).toThrow('redefining eval is deprecated');
     });

  it('Debe haber un error al redefinir arguments como ' +
     'argumento de una función', function() {
       expect(function() {
         eval('function f(arguments) {};');
       }).toThrow('redefining arguments is deprecated');
     });

  it('Debe haber un error al tener dos argumentos con el ' +
     'mismo nombre', function() {
       expect(function() {
         eval('function f(x, x) { }');
       }).toThrow('duplicate formal argument x');
     });

  it('Debe haber un error al acceder a la propiedad ' +
     'caller de una función', function() {
       expect(function() {
         eval('(function(){}).caller;');
       }).toThrow('\'caller\', \'callee\', and \'arguments\' properties may ' +
               'not be accessed on strict mode functions or the arguments ' +
               'objects for calls to them');
     });

  it('Debe haber un error al acceder a la propiedad arguments' +
     'de una función', function() {
       expect(function() {
         eval('(function(){}).arguments;');
       }).toThrow('\'caller\', \'callee\', and \'arguments\' properties may ' +
               'not be accessed on strict mode functions or the arguments ' +
               'objects for calls to them');
     });

  it('Debe haber un error al redefinir eval como una función', function() {
    expect(function() {
      eval('function eval(){ };');
    }).toThrow('redefining eval is deprecated');
  });

  it('Debe haber un error al redefinir arguments como una función', function() {
    expect(function() {
      eval('function arguments(){ }');
    }).toThrow('redefining arguments is deprecated');
  });

  //No lanzan la excepcion (no dice en la pagina si es un error)
  it('', function() {
    expect(function() {
      eval('(function(x){ x = 2; return arguments[0] === 1; })(1);');
    }).toThrow('');
  });

  it('', function() {
    expect(function() {
      eval('(function(x){ arguments[0] = 2; return x === 1; })(1);');
    }).toThrow('');
  });

  it('', function() {
    expect(function() {
      eval('(function(){ return this === undefined; })()');
    }).toThrow('');
  });

  it('', function() {
    expect(function() {
      eval('(function(){ return this === undefined; }).call()');
    }).toThrow('');
  });
});

