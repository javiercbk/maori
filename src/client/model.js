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

//NAMESPACE PATTERN.
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
* Drawable Object constructor
*/
MAORI.model.Drawable = function(x, y){
  this.x = x;
  this.y = y;
  this.draw = function(){/*Do Nothing function*/};
  this.isTouched = function(x, y) { return false; };
  this.getRectangle = function(){ return 0; };
}


/**
* @constructor
* Raindrop Constructor
*/
MAORI.model.Raindrop = function(x, y, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.currentAlpha = 1.0;
  this.currentSize = 10;

  this.draw = function() {
    //hardcoded color
    this.drawingContext.strokeStyle = 'rgba(70,213,222, ' + this.currentAlpha + ')';
    this.drawingContext.beginPath();
    this.drawingContext.arc(this.x, this.y, this.currentSize, 0, Math.PI * 2, true);
    this.drawingContext.closePath();
    this.drawingContext.stroke();
    this.currentAlpha = this.currentAlpha - 0.05;
    if((this.currentAlpha - 0.01) <= 0.00){
      this.currentAlpha = 0;
    }
    this.currentSize += 1;
    if(this.isNonVisible()){
      MAORI.model.removeDrawable(this);
    }
  };

  this.isNonVisible = function(){
    return this.currentAlpha <= 0.00;
  };
};


/**
* Sets up the Raindrop prototype
*/
MAORI.model.Raindrop.prototype = new MAORI.model.Drawable();


/**
* @constructor
* 
*/
MAORI.model.File = function(x, y, imagesrc, file, ctx) {
  var xOffset = 17;
  var yOffset = 11;
  this.x = x;
  this.y = y;
  this.file = file;
  this.text = new MAORI.model._createText(x + xOffset, y + yOffset, this.file.name);
  this.drawingContext = ctx;
  this.image = new Image();
  this.image.src = imagesrc;

  this.draw = function() {   
    //quizas haya que ponerlo el this.image.onload
    this.text.draw();
    this.drawingContext.drawImage(this.image, x, y);
    this.drawingContext.stroke();
  };

  this.nameSizeInPixels = function(){
    this.file.length * letterSize + xOffset;
  };

  this.isTouched = function(x, y){
    return ((Math.abs(this.x - x) < 10 &&
             Math.abs(this.y - y) < 10) ||
            this.text.isTouched);
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.File.prototype = MAORI.model.Drawable;



/**
* @constructor
*/
MAORI.model.Text = function(x, y, text, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.text = text;
  
  var rectX = 10;
  var rectY = 10;

  this.draw = function() {
    this.drawingContext.font = '12px Arial'
    this.drawingContext.fillStyle = '#74DF00'
    this.drawingContext.fillText(this.text, x, y)
    this.drawingContext.stroke();
  };

  this.isTouched = function(x, y){
    var size = ctx.measureText(this.text).width;
    return ((Math.abs(size - x) < 10 ||
             Math.abs(this.x - x) < 10) &&
            Math.abs(this.y - y) < 10);
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.Text.prototype = MAORI.model.Drawable;



/**
* @constructor
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
    ctx.strokeStyle = "#2E5CBA";
    ctx.stroke();
  };
};


/**
* Sets up the Line prototype
*/
MAORI.model.Line.prototype = MAORI.model.Drawable;



/**
* @constructor
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
MAORI.model.Arrow.prototype = MAORI.model.Line;



/**
* @constructor
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
MAORI.model.TwoSidedArrow.prototype = MAORI.model.Arrow;


/**
* Adds a drawable object to list of drawables
*/
MAORI.model.addDrawable = function(drawable){
  MAORI.model.drawables.push(drawable);
};


/**
* Removes a drawable object to list of drawables
*/
//TODO: make it more efficient
MAORI.model.removeDrawable = function(drawable){
  var drawableIndex = MAORI.model.drawables.indexOf(drawable);
  var drawableSize = MAORI.model.drawables.length;
  if(drawableIndex === 0){
    MAORI.model.drawables.shift();
  }else if(drawableIndex > 0){
    //last element
    if(drawableIndex === (drawableSize -1)){
      MAORI.model.drawables.pop();
    }else{
      var temp = [];
      for(var i = 0; i < drawableSize; i++){
        if(i != drawableIndex){
          temp.push(MAORI.model.drawables[i]);
        }
      }
      MAORI.model.drawables = temp;
    }
    if(!MAORI.model.checkForAnimation()){
      MAORI.event.fireEvent(MAORI.event.stopAnimation, document, null);
    }
  }
  return drawableIndex;
};


/**
* Creates a Raindrop to be displayed
*/
MAORI.model.createRaindrop = function(x,y){
  var x = event.properties.x;
  var y = event.properties.y;
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var rainDrop = new MAORI.model.Raindrop(x,y,ctx);
  MAORI.model.addDrawable(rainDrop);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
};


/**
* Finds wether an element was clicked
*/
MAORI.model.clickedAnyElement = function(event){
  var x = event.properties.x;
  var y = event.properties.y;
  var drawableSize = MAORI.model.drawables.length;
  for(var i = 0; i < drawableSize; i++){
    var drawable = MAORI.model.drawables[i];
    if(drawable.isTouched(x, y)){
      MAORI.model.createRaindrop(drawable.x, drawable.y);
    }
  }
};


/**
* Creates a file to be displayed from event
*/
MAORI.model.createFile = function(event){
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var file = event.properties.file;
  MAORI.model._createFile(x, y, file);
};


/**
* Creates a file to be displayed
*/
MAORI.model._createFile = function(x, y, f){
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var file = new MAORI.model.File(x,y,'file_icon.gif', f, ctx);
  var raindrop = new MAORI.model.Raindrop(x + 7 ,y + 7, ctx);
  MAORI.model.addDrawable(file);
  MAORI.model.addDrawable(raindrop);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
  return file;
};


/**
* Creates a Text to be diplayed from event
*/
MAORI.model.createText = function(event) {
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var textString = event.properties.text;
  MAORI.model._createText(x, y, textString);
};


/**
* Creates a Text to be diplayed
*/
MAORI.model._createText = function(x, y, textString) {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var text = new MAORI.model.Text(x, y, textString, ctx);
  MAORI.model.addDrawable(text);
  MAORI.event.fireEvent(MAORI.event.paint, document, null);
  return text;
};


/**
* Checks whether an Animable Object is 
* being displayed
* @returns true if animation is being displayed
* otherwise false
*/
MAORI.model.checkForAnimation = function(){
  var drawableSize = MAORI.model.drawables.length;
  for(var i = 0; i < drawableSize; i++){
    if(MAORI.model.drawables[i].hasOwnProperty('isNonVisible')){
      return true;
    }
  }
  return false;
};


/**
* Initializes model module
*/
MAORI.model.init = function(){
  document.addEventListener(MAORI.event.clicked, this.clickedAnyElement, false);
  document.addEventListener(MAORI.event.fileDropped, this.createFile, false);
  document.addEventListener(MAORI.event.textDropped, this.createText, false);
}