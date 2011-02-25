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
    drawable.draw();
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
