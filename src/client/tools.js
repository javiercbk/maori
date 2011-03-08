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
* Div element to add tools
*/
MAORI.tools.toolbox = {};



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
    MAORI.event.fireEvent(MAORI.event.textDropped, document, properties);
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

  this.workCallback = function(file) {
    var targetPoint = {x: this.x, y: this.y};
    var properties = {point: targetPoint, data: file};
    MAORI.event.fireEvent(MAORI.event.fileDropped, document, properties);
  };
};



/**
* @constructor
* Relation Creator tool used to relate
* drawables
*/
MAORI.tools.relationCreator = function() {
  this.work = function(selected) {
    for (var i = 0; i < selected.length; i++) {

    }
  };
};


/**
* Adds a tool to the ToolBox
* @param {String} imgPath as String.
* @param {Function} onClickFun to be executed.
*/
MAORI.tools.addTool = function(imgPath, onClickFun) {
  var toolImage = document.createElement('IMG');
  toolImage.src = imgPath;
  toolImage.onclick = onClickFun;
  toolImage.setAttribute('class', 'tool');
  MAORI.tools.toolbox.appendChild(toolImage);
};


/**
* Adds the relate tool to the toolbox
*/
MAORI.tools.addRelateTool = function() {
  var onClickFun = function(event) {
    MAORI.event.fireEvent(MAORI.event.relateSelected, document, null);
  }
  MAORI.tools.addTool('images/relate-tool.png', onClickFun);
};


/**
* Toolbox init function
* @param {String} toolbox id as String.
*/
MAORI.tools.init = function(toolbox) {
  MAORI.tools.toolbox = document.getElementById(toolbox);
  MAORI.tools.addRelateTool();
};
