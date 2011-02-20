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
MAORI.model.Raindrop.prototype = MAORI.model.Drawable;


/**
* @constructor
* 
*/
MAORI.model.File = function(x, y, imagesrc, file, ctx) {
  this.x = x;
  this.y = y;
  this.file = file;
  this.drawingContext = ctx;
  this.image = new Image();
  this.image.src = imagesrc;

  this.draw = function() {   
    //quizas haya que ponerlo el this.image.onload
    this.drawingContext.drawImage(this.image, x, y);
    this.drawingContext.fillText(this.file, x + 20, y + 11);
    this.drawingContext.stroke();
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.File.prototype = MAORI.model.Drawable;



/**
* @constructor
*/
MAORI.model.Box = function(x, y, text, ctx) {
  this.x = x;
  this.y = y;
  this.drawingContext = ctx;
  this.text = text;

  this.draw = function() {
    ctx.font = '12px Arial'
    ctx.fillStyle = '#74DF00'
    ctx.fillText(this.text, x, y += 20)
    ctx.fillRect(x - 20, y - 10, 10, 10)
    ctx.stroke();
  };
};


/**
* Sets up the File prototype
*/
MAORI.model.Box.prototype = MAORI.model.Drawable;



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
    //implement line drawing
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
MAORI.model.createRaindrop = function(event){
  var x = event.properties.x;
  var y = event.properties.y;
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var rainDrop = new MAORI.model.Raindrop(x,y,ctx);
  MAORI.model.addDrawable(rainDrop);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
};


/**
* Creates a file to be displayed
*/
MAORI.model.createFile = function(event){
  var x = event.properties.point.x;
  var y = event.properties.point.y;
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  var f = event.properties.file;
  var file = new MAORI.model.File(x,y,'file_icon.gif', f, ctx);
  var raindrop = new MAORI.model.Raindrop(x + 7 ,y + 7, ctx);
  MAORI.model.addDrawable(file);
  MAORI.model.addDrawable(raindrop);
  //MAORI.event.fireEvent(MAORI.event.paint, document, null);
  MAORI.event.fireEvent(MAORI.event.animate, document, null);
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
  document.addEventListener(MAORI.event.clicked, this.createRaindrop, false);
  document.addEventListener(MAORI.event.objectDropped, this.createFile, false);
}