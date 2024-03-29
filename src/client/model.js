/*
* Copyright (C) 2011  Javier Lecuona
*
* This file is part of Maori.
*
* Maori is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Maori is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

'use strict';


/**
* Maori namespace pattern.
*/
var MAORI = MAORI || {};


/**
* model module
*/
MAORI.model = {};


/**
* Non Movable Object to be painted in the canvas
*/
MAORI.model.drawables = [[]];


/**
* Selected elements on the canvas
*/
MAORI.model.selected = [];


/**
* Drawing canvas size
*/
MAORI.model.canvasSize = {x: 0, y: 0};


/**
* Current display rectangle
*/
MAORI.model.displaying = {x: 0 , y: 0};


/**
* Flag to determine if there is a drag operation
* currently being executed
*/
MAORI.model.dragState = false;


/**
* Signals if dragging Drawables
*/
MAORI.model.dragDrawables = false;


/**
* Raindrop activation
*/
MAORI.model.RAINDROP = false;


/**
* Remembers the drag start point
* to decide if it was a click or a drag
*/
MAORI.model.dragStartedPoint = {x: -1, y: -1};


/**
* Current scale for he drawing canvas.
*/
MAORI.model.currentScale = 0;



/**
* @constructor
* Drawable Object constructor.
* @param {Integer} x position.
* @param {Integer} y position.
*/
MAORI.model.Drawable = function(x, y) {
  this.composite = [];
  this.x = x;
  this.y = y;
  this.scale = 1;
  this.rotation = 0;
  this.draw = function() {/*Do nothing function*/};
  this.isTouched = function(x, y) { return false; };
  this.getRectangle = function() { return 0; };
  this.post = function() {/*Do nothing function*/};
  this.scale = function(s) {/*Do nothing function*/};
  this.isTouched = function(x, y) { return false; };
  this.rotate = function(radian) {
    this.rotation = radian;
  };
  this.move = function(moveToX, moveToY) {
    this.x = moveToX;
    this.y = moveToY;
    if (this.decorator != null) {
      this.decorator.move(moveToX, moveToY);
    }
  }
  this.decorator = null;
};



/**
* @constructor
* Circle Constructor
* @param {Integer} x position.
* @param {Integer} y position.
* @param {Float} radius of the circle.
* @param {Object} color of the circle.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Circle = function(x, y, radius, color, layer, ctx) {
  this.x = x;
  this.y = y;
  this.color = color;
  this.radius = radius;
  this.drawingContext = ctx;
  this.layer = layer;

  this.draw = function() {
    var colorStyle = 'rgba(' + this.color.r + ',' +
        this.color.g + ',' + this.color.b + ',' +
        this.color.alpha + ')';
    this.drawingContext.strokeStyle = colorStyle;
    this.drawingContext.beginPath();
    this.drawingContext.arc(this.x, this.y, this.radius, 0,
                            Math.PI * 2, true);
    this.drawingContext.closePath();
  };

  this.scale = function(s) {
    this.radius += scale;
  };
};



/**
* @constructor
* Raindrop Constructor
* @param {Integer} x position.
* @param {Integer} y position.
* @param {Object} color rgb.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Raindrop = function(x, y, color, layer, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  var currentAlpha = 1.0;
  var currentSize = 10;
  this.color = color;
  this.layer = layer

  this.draw = function() {
    this.color.alpha = currentAlpha;
    var circle = new MAORI.model.Circle(this.x, this.y,
      currentSize, this.color, 1,this.drawingContext);
    circle.draw();
    currentAlpha = currentAlpha - 0.05;
    if ((currentAlpha - 0.01) <= 0.00) {
      currentAlpha = 0;
    }
    currentSize += 1;
    if (this.isNonVisible()) {
      MAORI.model.removeDrawable(this, this.layer);
    }
  };

  this.scale = function(s) {
    currentSize += scale;
  };

  this.move = function(moveToX, moveToY) {
    this.__proto__.move.apply(this, [moveToX, moveToY]);
  };

  this.isNonVisible = function() {
    return currentAlpha <= 0.00;
  };
};


/**
* Sets up the Raindrop prototype
*/
MAORI.model.Raindrop.prototype = new MAORI.model.Drawable();


/**
* Sets up the Raindrop constructor
*/
MAORI.model.Raindrop.constructor = MAORI.model.Raindrop;



/**
* @constructor
* @param {Integer} x position.
* @param {Integer} y position.
* @param {String} imagesrc source to request the image.
* @param {Object} file to upload.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.File = function(x, y, imagesrc, file, layer, ctx) {
  var xOffset = 17;
  var yOffset = 11;
  this.x = x;
  this.y = y;
  this.file = file;
  this.text = new MAORI.model.Text(x + xOffset, y + yOffset,
                                   this.file.name, ctx);
  this.drawingContext = ctx;
  this.image = new Image();
  this.image.src = imagesrc;
  this.decorator = null;
  this.layer = layer;

  this.getRectangle = function() {
    var height = this.image.height;
    var width = this.image.width;
    var textRec = this.text.getRectangle();
    var textX = (textRec.x2 - textRec.x1);
    var textY = (textRec.y2 - textRec.y1);
    return {x1: this.x,
      y1: this.y,
      x2: this.x + width + textX,
      y2: this.y + Math.max(textY, height)};
  };

  this.move = function(moveToX, moveToY) {
    this.text.move(moveToX + xOffset, moveToY + yOffset);
    this.__proto__.move.apply(this, [moveToX, moveToY]);
  };

  this.draw = function() {
    //it would be better if drawImage
    //was called after image.onload
    this.text.draw();
    this.drawingContext.drawImage(this.image, this.x, this.y);
    if (this.decorator != null) {
      this.decorator.draw();
    }
  };

  this.isTouched = function(x, y) {
    var rectangle = this.getRectangle();
    return ((x < rectangle.x2 && x > rectangle.x1) &&
            (y < rectangle.y2 && y > rectangle.y1));
  };

  this.scale = function(s) {
    this.text.scale(s);
    this.image.scale(s);
    if (this.decorator != null) {
      this.decorator.scale(s);
    }
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.File.prototype = new MAORI.model.Drawable();


/**
* Sets up the File constructor
*/
MAORI.model.File.constructor = MAORI.model.File;



/**
* @constructor
* @param {Integer} x position.
* @param {Integer} y position.
* @param {String} text to be shown.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Text = function(x, y, text, layer, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.text = text;
  //20 chars
  var MAX_LENGTH = 20;

  var rectX = 10;
  var rectY = 10;
  var fontSize = 12;

  this.decorator = null;
  this.layer = layer;

  this.draw = function() {
    this.drawingContext.font = fontSize + 'px Arial';
    this.drawingContext.fillStyle = '#74DF00';
    this.drawingContext.fillText(this.text, this.x, this.y);
    if (this.decorator != null) {
      this.decorator.draw();
    }
  };

  this.getRectangle = function() {
    var measures = ctx.measureText(this.text);
    var textX2 = measures.width;
    return {x1: this.x,
      x2: textX2 + this.x,
      //1.5 is an empiric value. Should investigate
      //a little more about fonts
      y1: this.y - (fontSize / 1.5),
      y2: this.y};
  };

  this.isTouched = function(x, y) {
    var rectangle = this.getRectangle();
    return ((x < rectangle.x2 && x > rectangle.x1) &&
            (y < rectangle.y2 && y > rectangle.y1));
  };

  this.move = function(moveToX, moveToY) {
    this.__proto__.move.apply(this, [moveToX, moveToY]);
  };

  this.scale = function(s) {
    fontSize += s;
    if (this.decorator !== null) {
      this.decorator.scale(s);
    }
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.Text.prototype = new MAORI.model.Drawable();


/**
* Sets up the Text constructor
*/
MAORI.model.Text.constructor = MAORI.model.Text;



/**
* @constructor
* @param {Object} from drawable.
* @param {Object} to drawable.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Line = function(from, to, layer, ctx) {
  this.from = from;
  this.to = to;
  this.drawingContext = ctx;
  this.decorator = null;
  this.thickness = 1;
  this.layer = layer;

  this.draw = function() {
    ctx.beginPath();
    var line = this.calculateLine();
    ctx.lineWidth = 3;
    ctx.moveTo(line.x1, line.y1);
    ctx.lineTo(line.x2, line.y2);
    ctx.strokeStyle = '#2E5CBA';
    ctx.closePath();
  };

  var calculateCenter = function(element) {
    var cx = -1;
    var cy = -1;
    var r = element.getRectangle();
    cx = (r.x1 + r.x2) / 2;
    cy = (r.y1 + r.y2) / 2;
    return {x: cx, y: cy};
  };

  this.calculateLine = function() {
    var c1 = calculateCenter(this.from);
    var c2 = calculateCenter(this.to);
    return {x1: c1.x, x2: c2.x, y1: c1.y, y2: c2.y};
  };

  this.scale = function(s) {
    this.thickness += s;
    if (thickness < 1) {
      thickness = 1;
    }
  };

};


/**
* Sets up the Line prototype
*/
MAORI.model.Line.prototype = new MAORI.model.Drawable();


/**
* Sets up the Line constructor
*/
MAORI.model.Line.constructor = MAORI.model.Line;



/**
* @constructor
* @param {Object} rectangle to be decorated.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.BoxDecorator = function(rectangle, ctx) {
  this.x = rectangle.x1;
  this.y = rectangle.y1;
  this.x2 = rectangle.x2;
  this.y2 = rectangle.y2;
  this.decorator = null;
  //5 pixel offset
  var offset = 5;

  this.setOffset = function(newOffset) {
    offset = newOffset;
  };

  this.drawingContext = ctx;

  this.draw = function() {
    ctx.beginPath();
    ctx.strokeStyle = '#E1D514';
    ctx.moveTo(this.x - offset, this.y - offset);
    ctx.lineTo(this.x2 + offset, this.y - offset);
    ctx.lineTo(this.x2 + offset, this.y2 + offset);
    ctx.lineTo(this.x - offset, this.y2 + offset);
    ctx.lineTo(this.x - offset, this.y - offset);
  };

  this.move = function(moveToX, moveToY) {
    var width = this.x2 - this.x;
    var height = this.y2 - this.y;
    this.x = moveToX;
    this.y = moveToY - height;
    this.x2 = width + moveToX;
    this.y2 = moveToY;
  };

  this.scale = function(s) {
    this.x += s;
    this.y += s + (1 * s / 2);
    this.x2 += s + (1 * s / 2);
    this.y2 += s + (1 * s / 2);
    offset += s;
    if (this.decorator !== null) {
      this.decorator.scale(s);
    }
  }
};


/**
* Sets up the BoxDecorator prototype
*/
MAORI.model.BoxDecorator.prototype = new MAORI.model.Drawable();


/**
* Sets up the BoxDecorator constructor
*/
MAORI.model.BoxDecorator.constructor = MAORI.model.BoxDecorator;



/**
* @constructor
* @param {Object} rectangle to be decorated.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.SpecialBoxDecorator = function(rectangle, ctx) {
  this.x = rectangle.x1;
  this.y = rectangle.y1;
  this.x2 = rectangle.x2;
  this.y2 = rectangle.y2;
  this.decorator = null;
  //5 pixel offset
  var offset = 5;
  var backForth = new MAORI.BackForth([1, 2, 3, 4, 5]);

  this.setOffset = function(newOffset) {
    offset = newOffset;
  };

  this.drawingContext = ctx;

  this.draw = function() {
    ctx.beginPath();
    var currentOffset = offset + backForth.next();
    ctx.moveTo(this.x - currentOffset, this.y - currentOffset);
    ctx.lineTo(this.x2 + currentOffset, this.y - currentOffset);
    ctx.lineTo(this.x2 + currentOffset, this.y2 + currentOffset);
    ctx.lineTo(this.x - currentOffset, this.y2 + currentOffset);
    ctx.lineTo(this.x - currentOffset, this.y - currentOffset);
    ctx.strokeStyle = '#E1D514';
  };

  this.move = function(moveToX, moveToY) {
    this.__proto__.move.apply(this, [moveToX, moveToY]);
  };

  this.scale = this.__proto__.scale;
};


/**
* Sets up the BoxDecorator prototype
*/
MAORI.model.SpecialBoxDecorator.prototype =
    new MAORI.model.BoxDecorator({x1: 0, y1: 0, x2: 0, y2: 0});


/**
* Sets up the BoxDecorator constructor
*/
MAORI.model.SpecialBoxDecorator.constructor = MAORI.model.SpecialBoxDecorator;



/**
* @constructor
* @param {Object} from drawable.
* @param {Object} to drawable.
* @param {Integer} layer where it belongs.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Arrow = function(from, to, layer, ctx) {
  this.from = from;
  this.to = to;
  this.drawingContext = ctx;
  this.thickness = 1;
  this.layer = layer;

  this.draw = function() {
    ctx.beginPath();
    var line = this.__proto.calculateLine.apply(this, []);
    ctx.moveTo(line.x1, line.y1);
    ctx.lineTo(line.x2, line.y2);
    //TODO: Add triangle at the end of the line
    ctx.strokeStyle = '#2E5CBA';
  };

  this.scale = this.__proto__.scale;
};


/**
* Sets up the Arrow prototype
*/
MAORI.model.Arrow.prototype = new MAORI.model.Line();


/**
* Sets up the Arrow constructor
*/
MAORI.model.Arrow.constructor = MAORI.model.Arrow;



/**
* @constructor
* @param {Integer} fromX origin.
* @param {Integer} fromY origin.
* @param {Integer} toX destiny.
* @param {Integer} toY destiny.
* @param {Integer} layer where it belongs. 
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.TwoSidedArrow = function(fromX, fromY, toX, toY, layer, ctx) {
  this.x1 = fromX;
  this.y1 = fromY;
  this.x2 = toX;
  this.y2 = toY;
  this.decorator = null;
  this.drawingContext = ctx;
  this.layer = layer;

  this.draw = function() {
    //implement line drawing + arrow
  };

  this.scale = this.__proto__.scale;
};


/**
* Sets up the TwoSidedArrow prototype
*/
MAORI.model.TwoSidedArrow.prototype = new MAORI.model.Arrow();


/**
* Sets up the TwoSidedArrow constructor
*/
MAORI.model.TwoSidedArrow.constructor = MAORI.model.TwoSidedArrow;


/**
* Adds a drawable object to list of drawables to
* be rendered.
* @param {Object} drawable to add.
* @param {Integer} layer where to be added.
*/
MAORI.model.addDrawable = function(drawable, layer) {
  var j = layer;
  while(j >= 0){
    if(MAORI.model.drawables[j] === undefined){
      //create empty "layer"
      MAORI.model.drawables.push([]);
    }
    j--;
  }
  MAORI.model.drawables[layer].push(drawable);
};


/**
* Removes a drawable object to list of drawables
* @param {Object} drawable to be removed.
* @return {Integer} index of the removed drawable.
*/
//TODO: make it more efficient
MAORI.model.removeDrawable = function(drawable, layer) {
  var drawableIndex = MAORI.model.drawables[layer].indexOf(drawable);
  var drawableSize = MAORI.model.drawables[layer].length;
  if (drawableIndex === 0) {
    MAORI.model.drawables[layer].shift();
  }else if (drawableIndex > 0) {
    //last element
    if (drawableIndex === (drawableSize - 1)) {
      MAORI.model.drawables[layer].pop();
    } else {
      var temp = [];
      for (var i = 0; i < drawableSize; i++) {
        if (i != drawableIndex) {
          temp.push(MAORI.model.drawables[i]);
        }
      }
      MAORI.model.drawables[layer] = temp;
    }
  }
  MAORI.model.stopAnimation();
  return drawableIndex;
};


/**
* Creates a Raindrop to be displayed
* @param {Integer} x position.
* @param {Integer} y position.
*/
MAORI.model.createRaindrop = function(x, y) {
  var x = event.properties.x;
  var y = event.properties.y;
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var color = {r: 70, g: 213, b: 222};
  var rainDrop = new MAORI.model.Raindrop(x, y, color, 1, ctx);
  MAORI.model.addDrawable(rainDrop,1);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
};


/**
* Renders a box decorator over a rectangle.
* @param {Object} drawable to be rendered.
*/
MAORI.model.createBoxDecorator = function(drawable) {
  var rectangle = drawable.getRectangle();
  var ctx = MAORI.draw.ctx;
  var box = new MAORI.model.BoxDecorator(rectangle, ctx);
  drawable.decorator = box;
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};


/**
* Creates a special decorator around a drawable.
* @param {Object} drawable to be decorated.
*/
MAORI.model.createSpecialDecorator = function(drawable) {
  var rectangle = drawable.getRectangle();
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var box = new MAORI.model.SpecialBoxDecorator(rectangle, ctx);
  drawable.decorator = box;
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
};


/**
* Calls performClick with decorate function
* @param {Object} event fired when click performed.
*/
MAORI.model.clickedAnyElement = function(event) {
  MAORI.model.performClick(event, MAORI.model.decorate);
};


/**
* Finds wether an element was clicked and executes
* the given function over it.
* @param {Object} event fired when click performed.
* @param {Function} func to perform to drawable.
*/
MAORI.model.performClick = function(event, func) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  //console.log('clicked x:' + x + ' y: ' + y);
  //Apply multi select
  if (!event.ctrlKey) {
    MAORI.model.clearSelected();
  }
  for(var j = MAORI.model.drawables.length - 1; j >= 0 ; j--){
    for (var i = 0; i < MAORI.model.drawables[j].length; i++) {
      var drawable = MAORI.model.drawables[j][i];
      if (drawable.isTouched(x, y)) {
        func(drawable);
        //avoid keeping evaluating
        return;
      }
    }
  }
};


/**
* Decorates drawable with BoxDecorator
* @param {Object} drawable to decorate.
*/
MAORI.model.decorate = function(drawable) {
  MAORI.model.createBoxDecorator(drawable);
  MAORI.model.selected.push(drawable);
};


/**
* Decorates drawable with BoxDecorator
* @param {Object} drawable to decorate.
*/
MAORI.model.specialAction = function(drawable) {
  MAORI.model.createSpecialDecorator(drawable);
  MAORI.model.selected.push(drawable);
};


/**
* Clears all BoxDecorator objects from the
* list to be rendered.
*
*/
MAORI.model.clearSelected = function() {
  for (var i = 0; i < MAORI.model.selected.length; i++) {
    var drawable = MAORI.model.selected[i];
    drawable.decorator = null;
  }
  MAORI.model.selected = [];
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};


/**
* Creates a file to be displayed from event
* @param {Object} event fired when file dropped.
*/
MAORI.model.createFile = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var file = event.properties.file;
  MAORI.model._createFile(x, y, file);
};


/**
* Creates a file to be displayed
* @param {Integer} x position.
* @param {Integer} y position.
* @param {Object} f is the file to be uploaded.
* @return {Object} file created.
*/
MAORI.model._createFile = function(x, y, f) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var file = new MAORI.model.File(x, y, 'images/file_icon.gif', f, 0, ctx);
  var color = {r: 70, g: 213, b: 222};
  var raindrop = new MAORI.model.Raindrop(x + 7 , y + 7, color, 1, ctx);
  MAORI.model.addDrawable(file,0);
  MAORI.model.addDrawable(raindrop,1);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
  return file;
};


/**
* Creates a Text to be diplayed from event
* @param {Object} event fired when text dropped.
*/
MAORI.model.createText = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var data = event.dataTransfer.getData('text');
  var textString = data;
  MAORI.model._createText(x, y, textString);
};


/**
* Creates a Text to be diplayed
* @param {Integer} x position.
* @param {Integer} y position.
* @param {String} textString text to be rendered.
* @return {Object} text created.
*/
MAORI.model._createText = function(x, y, textString) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var text = new MAORI.model.Text(x, y, textString, 0, ctx);
  var color = {r: 70, g: 213, b: 222};
  var raindrop = new MAORI.model.Raindrop(x + 7, y, color, 1, ctx);
  MAORI.model.addDrawable(text,0);
  MAORI.model.addDrawable(raindrop,1);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
  return text;
};


/**
* Checks whether an Animable Object is
* being displayed
* @return {Boolean} true if animation is being displayed otherwise false.
*/
MAORI.model.checkForAnimation = function() {
  for(var j = MAORI.model.drawables.length - 1; j >= 0 ; j--){
    for (var i = 0; i < MAORI.model.drawables[j].length; i++) {
      var drawable = MAORI.model.drawables[j][i];
      if (drawable.hasOwnProperty('isNonVisible')) {
        return true;
      }
    }
  }
  return false;
};


/**
* function called when drag starts
* @param {Event} event mouseDown.
*/
MAORI.model.dragStart = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  //console.log('dragStart x:' + x + ' y: ' + y);
  MAORI.model.dragStartedPoint.x = x;
  MAORI.model.dragStartedPoint.y = y;
  MAORI.model.dragState = true;
  for(var j = MAORI.model.drawables.length - 1; j >= 0 ; j--){
    for (var i = 0; i < MAORI.model.drawables[j].length; i++) {
      var drawable = MAORI.model.drawables[j][i];
      if (drawable.isTouched(x, y)) {
        MAORI.model.dragDrawables = true;
        return;
      }
    }
  }
};


/**
* function called when drag stops.
* @param {Event} event mouseUp.
*/
MAORI.model.dragStop = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  //console.log('dragStop x:' + x + ' y: ' + y);
  if (MAORI.model.dragStartedPoint.x === x &&
      MAORI.model.dragStartedPoint.y === y) {
    //it was a click so ignore call
    MAORI.model.clearDragStarted();
    return;
  }
  for (var i = 0; i < MAORI.model.dragDrawables.length; i++) {
    var drawable = MAORI.model.dragDrawables[i];
    drawable.x = event.properties.point.x;
    drawable.y = event.properties.point.y;
  }
  MAORI.model.clearDragStarted();
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};


/**
* Function called on meaningful mouse Move.
* @param {Event} event mouseMove.
*/
MAORI.model.mouseMove = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var offsetX = x - MAORI.model.dragStartedPoint.x;
  var offsetY = y - MAORI.model.dragStartedPoint.y;
  MAORI.model.dragStartedPoint.x = x;
  MAORI.model.dragStartedPoint.y = y;
  if (MAORI.model.dragDrawables) {
    //dragging drawables
    for (var i = 0; i < MAORI.model.selected.length; i++) {
      var drawable = MAORI.model.selected[i];
      var newX = drawable.x + offsetX;
      var newY = drawable.y + offsetY;
      drawable.move(newX, newY);
    }
  } else {
    //moving display region
    for(var j = MAORI.model.drawables.length - 1; j >= 0 ; j--){
      for (var i = 0; i < MAORI.model.drawables[j].length; i++) {
        var drawable = MAORI.model.drawables[j][i];
        var newX = drawable.x + offsetX;
        var newY = drawable.y + offsetY;
        drawable.move(newX, newY);
      }
    }
    MAORI.model.displaying.x = MAORI.model.displaying.x + offsetX;
    MAORI.model.displaying.y = MAORI.model.displaying.y + offsetY;
  }
  if (MAORI.event.mousePressedState) {
    MAORI.event.fireEvent(MAORI.event.repaint, document, null);
  }
};


/**
* Initializes model module
* @this {Module}
*/
MAORI.model.init = function() {
  var canvas = document.getElementById('maori-canvas');
  MAORI.model.canvasSize = {x: canvas.width, y: canvas.heigth};
  //initial coordenates
  MAORI.model.displaying = {x1: 0, x2: canvas.width, y1: 0, y2: canvas.heigth};
  document.addEventListener(MAORI.event.textDropped,
                            MAORI.model.createText, false);
  document.addEventListener(MAORI.event.fileDropped,
                            MAORI.model.createFile, false);
  document.addEventListener(MAORI.event.relateSelected,
                            MAORI.model.relateSelected, false);
  document.addEventListener(MAORI.event.parentSelected,
                            MAORI.model.parentSelected, false);
};


/**
* Assigns invalid values to dragStarted
*/
MAORI.model.clearDragStarted = function() {
  MAORI.model.dragStartedPoint.x = -1;
  MAORI.model.dragStartedPoint.y = -1;
  MAORI.model.dragDrawables = false;
};


/**
* Performs an special operation
* @param {Event} event with properties.
*/
MAORI.model.specialOperation = function(event) {
  MAORI.model.performClick(event, MAORI.model.specialAction);
};


/**
* Stops animation by firing an event. If no animation
* is being displayed, it will do nothing.
*/
MAORI.model.stopAnimation = function() {
  if (!MAORI.model.checkForAnimation()) {
    MAORI.event.fireEvent(MAORI.event.stopAnimation, document, null);
  }
};


/**
* Relates the selected Elements
*/
MAORI.model.relateSelected = function() {
  for (var i = 1; i < MAORI.model.selected.length; i++) {
    MAORI.model.relateElement(MAORI.model.selected[i - 1],
                              MAORI.model.selected[i]);
  }
};


/**
* Relates two elements
* @param {Object} e1 to be related with e2.
* @param {Object} e2 to be related with e1.
*/
MAORI.model.relateElement = function(e1, e2) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var line = new MAORI.model.Line(e1, e2, 2, ctx);
  MAORI.model.addDrawable(line,2);
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};


/**
* Parent the selected Elements
*/
MAORI.model.parentSelected = function() {
  if (MAORI.model.selected.length > 0) {
    var parent = MAORI.model.selected[0];
    for (var i = 1; i < MAORI.model.selected.length; i++) {
      MAORI.model.parentElement(parent,
                                MAORI.model.selected[i]);
    }
  }
};


/**
* Creates a Parent Child relation between
* two elements
* @param {Object} e1 to be Parent of e2.
* @param {Object} e2 to be Child of e1.
*/
MAORI.model.parentElement = function(e1, e2) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var arrow = new MAORI.model.Arrow(e1, e2, ctx);
  MAORI.model.addDrawable(line,3);
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};


/**
* Scales all drawables.
* @param {Integer} s scale to apply.
*/
MAORI.model.scaleAll = function(s) {
  MAORI.model.currentScale += s;
  for(var j = MAORI.model.drawables.length - 1; j >= 0 ; j--){
      for (var i = 0; i < MAORI.model.drawables[j].length; i++) {
        MAORI.model.drawables[j][i].scale(s);
      }
  }
  MAORI.event.fireEvent(MAORI.event.repaint, document, null);
};
