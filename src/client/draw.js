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
* draw module
*/
MAORI.draw = {};


/**
* Current animation id
*/
MAORI.draw.animationId = -1;


/**
* Current drawing stack
*/
MAORI.draw.drawingStack = 0;


/**
* Starts an animation, if an animation
* is currently in progress it will do
* nothing.
*/
MAORI.draw.animate = function() {
  if (MAORI.draw.animationId < 0) {
    MAORI.draw.animationId = setInterval(MAORI.draw.repaint, 50);
  }
};


/**
* Stops the current animation, if no animation
* is being displayed it will do nothing.
*/
MAORI.draw.stopAnimation = function() {
  if (MAORI.draw.animationId > 0) {
    clearInterval(MAORI.draw.animationId);
    MAORI.draw.animationId = -1;
    MAORI.draw.repaint();
  }
};


/**
* Draws all drawable registered without clearing the canvas
*/
MAORI.draw.paint = function() {
  //Cycle through all drawable
  for (var i = 0; i < MAORI.model.drawables.length; i++) {
    var drawable = MAORI.model.drawables[i];
    var ctx = MAORI.general.drawingCanvas.getContext('2d');
    if (drawable.hasOwnProperty('getRectangle')) {
      var r = drawable.getRectangle();
      var o = 10 + MAORI.model.currentScale;
      ctx.fillStyle = '#353333';
      ctx.fillRect(r.x1 - o, r.y1 - o, (r.x2 - r.x1) + o, (r.y2 - r.y1) + o);
      MAORI.draw.pushContext();
    }
    drawable.draw();
    ctx.stroke();
    MAORI.draw.popContext();
  }
};


/**
* Push the current context to the canvas drawing stack.
*/
MAORI.draw.pushContext = function() {
  var ctx = MAORI.general.drawingCanvas.getContext('2d');
  ctx.save();
  MAORI.draw.drawingStack++;
  ctx.globalAlpha = 1;
};


/**
* Pops a context from canvas stacks and stroke it.
*/
MAORI.draw.popContext = function() {
  if (MAORI.draw.drawingStack >= 0) {
    var ctx = MAORI.general.drawingCanvas.getContext('2d');
    ctx.restore();
    MAORI.draw.drawingStack--;
    ctx.stroke();
  }
};


/**
* Draws all drawable registered clearing the canvas
*/
MAORI.draw.repaint = function() {
  //clean canvas
  MAORI.general.drawingCanvas.width = MAORI.general.drawingCanvas.width;
  MAORI.draw.paint();
};


/**
* Initilize draw module
* @this {Module} MAORI.draw Module.
*/
MAORI.draw.init = function() {
  document.addEventListener(MAORI.event.repaint, this.repaint, false);
  document.addEventListener(MAORI.event.paint, this.paint, false);
  document.addEventListener(MAORI.event.animate, this.animate, false);
  document.addEventListener(MAORI.event.stopAnimation,
                            this.stopAnimation, false);
};
