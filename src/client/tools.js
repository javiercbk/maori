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
* Tools module
*/
MAORI.tools = {};



/**
* @constructor
* Text creator tool used for inserting text to
* canvas
*/
MAORI.tools.textCreator = function() {
  this.work = function(x, y) {
    this.x = x;
    this.y = y;
    var properties = {calledBy: this};
    MAORI.event.fireEvent(MAORI.event.showTextInput, document, properties);
  }

  this.workCallback = function(textString) {
    var targetPoint = {x: this.x, y: this.y};
    var properties = {point: targetPoint, text: textString};
    MAORI.event.fireEvent(MAORI.event.fileDropped, document, properties);
  };
};



/**
* @constructor
* File creator tool used a file to the canvas
*/
MAORI.tools.fileCreator = function() {
  this.work = function(x, y) {
    this.x = x;
    this.y = y;
    var properties = {calledBy: this};
    MAORI.event.fireEvent(MAORI.event.showTextInput, document, properties);
  }

  this.workCallback = function(textString) {
    var targetPoint = {x: this.x, y: this.y};
    var properties = {point: targetPoint, text: textString};
    MAORI.event.fireEvent(MAORI.event.fileDropped, document, properties);
  };
};
