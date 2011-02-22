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
MAORI.model.drawables = [];


/**
* Raindrop activation
*/
MAORI.model.RAINDROP = false;



/**
* @constructor
* Drawable Object constructor.
* @param {Integer} x position.
* @param {Integer} y position.
*/
MAORI.model.Drawable = function(x, y) {
  this.x = x;
  this.y = y;
  this.draw = function() {/*Do nothing function*/};
  this.isTouched = function(x, y) { return false; };
  this.getRectangle = function() { return 0; };
  this.post = function() {/*Do nothing function*/};
};



/**
* @constructor
* Raindrop Constructor
* @param {Integer} x position.
* @param {Integer} y position.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Raindrop = function(x, y, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.currentAlpha = 1.0;
  this.currentSize = 10;

  this.draw = function() {
    //hardcoded color
    this.drawingContext.strokeStyle = 'rgba(70,213,222, ' +
        this.currentAlpha + ')';
    this.drawingContext.beginPath();
    this.drawingContext.arc(this.x, this.y, this.currentSize, 0,
                            Math.PI * 2, true);
    this.drawingContext.closePath();
    this.drawingContext.stroke();
    this.currentAlpha = this.currentAlpha - 0.05;
    if ((this.currentAlpha - 0.01) <= 0.00) {
      this.currentAlpha = 0;
    }
    this.currentSize += 1;
    if (this.isNonVisible()) {
      MAORI.model.removeDrawable(this);
    }
  };

  this.isNonVisible = function() {
    return this.currentAlpha <= 0.00;
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
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.File = function(x, y, imagesrc, file, ctx) {
  var xOffset = 17;
  var yOffset = 11;
  this.x = x;
  this.y = y;
  this.file = file;
  this.text = new MAORI.model._createText(x + xOffset, y + yOffset,
                                          this.file.name);
  this.drawingContext = ctx;
  this.image = new Image();
  this.image.src = imagesrc;

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

  this.draw = function() {
    //it would be better if drawImage
    //was called after image.onload
    this.text.draw();
    this.drawingContext.drawImage(this.image, x, y);
    this.drawingContext.stroke();
  };

  this.isTouched = function(x, y) {
    return ((Math.abs(this.x - x) < 10 &&
             Math.abs(this.y - y) < 10) ||
            this.text.isTouched());
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
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Text = function(x, y, text, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.text = text;

  var rectX = 10;
  var rectY = 10;
  var fontSize = 12;

  this.draw = function() {
    this.drawingContext.font = fontSize + 'px Arial';
    this.drawingContext.fillStyle = '#74DF00';
    this.drawingContext.fillText(this.text, x, y);
    this.drawingContext.stroke();
  };

  this.getRectangle = function() {
    var measures = ctx.measureText(this.text);
    var textX2 = measures.width;
    return {x1: this.x,
      x2: textX2 + this.x,
      y1: this.y - (fontSize / 2),
      y2: this.y};
  };

  this.isTouched = function(x, y) {
    var size = ctx.measureText(this.text).width;
    var inXRect = false;
    if (x > this.x) {
      inXRect = Math.abs(x - this.x - size) < size + 5;
    } else {
      inXRect = Math.abs(this.x - x) < 5;
    }
    var inYRect = Math.abs(this.y - y) < 10;
    return inXRect && inYRect;
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
* @param {Integer} fromX origin.
* @param {Integer} fromY origin.
* @param {Integer} toX destiny.
* @param {Integer} toY destiny.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Line = function(fromX, fromY, toX, toY, ctx) {
  this.x1 = fromX;
  this.y1 = fromY;
  this.x2 = toX;
  this.y2 = toY;
  this.drawingContext = ctx;

  this.draw = function() {
    ctx.beginPath();
    ctx.moveTo(this.x1, this.y1);
    ctx.lineTo(this.x2, this.y2);
    ctx.strokeStyle = '#2E5CBA';
    ctx.stroke();
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
  //5 pixel offset
  var offset = 5;

  this.setOffset = function(newOffset) {
    offset = newOffset;
  };

  this.drawingContext = ctx;

  this.draw = function() {
    ctx.beginPath();
    ctx.moveTo(this.x - offset, this.y - offset);
    ctx.lineTo(this.x2 + offset, this.y - offset);
    ctx.lineTo(this.x2 + offset, this.y2 + offset);
    ctx.lineTo(this.x - offset, this.y2 + offset);
    ctx.lineTo(this.x - offset, this.y - offset);
    ctx.strokeStyle = '#E1D514';
    ctx.stroke();
  };
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
* @param {Integer} fromX origin.
* @param {Integer} fromY origin.
* @param {Integer} toX destiny.
* @param {Integer} toY destiny.
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.Arrow = function(fromX, fromY, toX, toY, ctx) {
  this.x1 = fromX;
  this.y1 = fromY;
  this.x2 = toX;
  this.y2 = toY;
  this.drawingContext = ctx;

  this.draw = function() {
    //implement line drawing + arrow
  };
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
* @param {Object} ctx 2D drawing context.
*/
MAORI.model.TwoSidedArrow = function(fromX, fromY, toX, toY, ctx) {
  this.x1 = fromX;
  this.y1 = fromY;
  this.x2 = toX;
  this.y2 = toY;
  this.drawingContext = ctx;

  this.draw = function() {
    //implement line drawing + arrow
  };
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
*/
MAORI.model.addDrawable = function(drawable) {
  MAORI.model.drawables.push(drawable);
};


/**
* Removes a drawable object to list of drawables
* @param {Object} drawable to be removed.
* @return {Integer} index of the removed drawable.
*/
//TODO: make it more efficient
MAORI.model.removeDrawable = function(drawable) {
  var drawableIndex = MAORI.model.drawables.indexOf(drawable);
  var drawableSize = MAORI.model.drawables.length;
  if (drawableIndex === 0) {
    MAORI.model.drawables.shift();
  }else if (drawableIndex > 0) {
    //last element
    if (drawableIndex === (drawableSize - 1)) {
      MAORI.model.drawables.pop();
    } else {
      var temp = [];
      for (var i = 0; i < drawableSize; i++) {
        if (i != drawableIndex) {
          temp.push(MAORI.model.drawables[i]);
        }
      }
      MAORI.model.drawables = temp;
    }
    if (!MAORI.model.checkForAnimation()) {
      MAORI.event.fireEvent(MAORI.event.stopAnimation, document, null);
    }
  }
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
  var rainDrop = new MAORI.model.Raindrop(x, y, ctx);
  MAORI.model.addDrawable(rainDrop);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
};


/**
* Renders a box decorator over a rectangle.
* @param {Object} rectangle to be rendered.
*/
MAORI.model.createBoxDecorator = function(rectangle) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var box = new MAORI.model.BoxDecorator(rectangle, ctx);
  MAORI.model.addDrawable(box);
  MAORI.event.fireEvent(MAORI.event.paint, document, null);
};


/**
* Finds wether an element was clicked
* @param {Object} event fired when click performed.
*/
MAORI.model.clickedAnyElement = function(event) {
  var x = event.properties.x;
  var y = event.properties.y;
  MAORI.model.clearSelected();
  var drawableSize = MAORI.model.drawables.length;
  //here I should ask if ctrl was pressed
  // in order to apply multi select
  MAORI.model.clearSelected();
  for (var i = 0; i < drawableSize; i++) {
    var drawable = MAORI.model.drawables[i];
    if (drawable.isTouched(x, y)) {
      MAORI.model.createBoxDecorator(drawable.getRectangle());
      //avoid keeping evaluating
      return;
    }
  }
};


/**
* Clears all BoxDecorator objects from the
* list to be rendered.
*
*/
MAORI.model.clearSelected = function() {
  var drawableSize = MAORI.model.drawables.length;
  for (var i = 0; i < drawableSize; i++) {
    var drawable = MAORI.model.drawables[i];
    if (drawable instanceof MAORI.model.BoxDecorator) {
      MAORI.model.removeDrawable(drawable);
      MAORI.event.fireEvent(MAORI.event.repaint, document, null);
      //I have to call it again since the array was changed
      MAORI.model.clearSelected();
    }
  }
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
  var file = new MAORI.model.File(x, y, 'file_icon.gif', f, ctx);
  var raindrop = new MAORI.model.Raindrop(x + 7 , y + 7, ctx);
  MAORI.model.addDrawable(file);
  MAORI.model.addDrawable(raindrop);
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
  var textString = event.properties.text;
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
  var text = new MAORI.model.Text(x, y, textString, ctx);
  MAORI.model.addDrawable(text);
  var raindrop = new MAORI.model.Raindrop(x + 7, y, ctx);
  MAORI.model.addDrawable(raindrop);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
  return text;
};


/**
* Checks whether an Animable Object is
* being displayed
* @return {Boolean} true if animation is being displayed otherwise false.
*/
MAORI.model.checkForAnimation = function() {
  var drawableSize = MAORI.model.drawables.length;
  for (var i = 0; i < drawableSize; i++) {
    if (MAORI.model.drawables[i].hasOwnProperty('isNonVisible')) {
      return true;
    }
  }
  return false;
};


/**
* Initializes model module
* @this {Module}
*/
MAORI.model.init = function() {
  document.addEventListener(MAORI.event.clicked, this.clickedAnyElement, false);
  document.addEventListener(MAORI.event.fileDropped, this.createFile, false);
  document.addEventListener(MAORI.event.textDropped, this.createText, false);
};
