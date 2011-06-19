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
* General Usage module
*/
MAORI.general = {};


/**
* Current Client Height
*/
MAORI.general.clientHeight = 0;


/**
* Current Client Width
*/
MAORI.general.clientWidth = 0;


/**
* Calculates the position of an HTML element
* @param {Element} elementId of the element.
* @return {Object} position of the given element
* or undefined if no element was found.
*/
MAORI.general.getPosition = function(elementId) {
  var element = document.getElementById(elementId);
  var position = {x: 0, y: 0};
  if (element == null) {
    return undefined;
  }
  position.x = element.offsetLeft;
  position.y = element.offsetTop;
  return position;
};


/**
* Show a text input on inputHelper div
* @param {Event} event showTextInput.
*/
MAORI.general.showTextInput = function(event) {
  var eventCreator = event.properties.calledBy;
  var inputHelperDiv = document.getElementBydId('inputHelper');
  var textInput = document.createElement('input');
  input.setAttribute('type', 'text');
  var buttonInput = document.createElement('button');
  buttonInput.onclick = eventCreator.workCallback(textInput.value);
  inputHelperDiv.appendChild(textInput);
  inputHelperDiv.appendChild(buttonInput);
};


/**
* Calculates the client Heigth and width
*/
MAORI.general.clientWH = function() {
  if (typeof (window.innerWidth) == 'number') {
    //Non-IE
    MAORI.general.clientWidth = window.innerWidth;
    MAORI.general.clientHeight = window.innerHeight;
  } else if (document.documentElement && (document.documentElement.clientWidth ||
					   document.documentElement.clientHeight )) {
    //IE 6+ in 'standards compliant mode'
    MAORI.general.clientWidth = document.documentElement.clientWidth;
    MAORI.general.clientHeight = document.documentElement.clientHeight;
  } else if (document.body && (document.body.clientWidth ||
			       document.body.clientHeight)) {
    //IE 4 compatible
    MAORI.general.clientWidth = document.body.clientWidth;
    MAORI.general.clientHeight = document.body.clientHeight;
  }
}


/**
* Calculates the proper size to each element of maori.
* The canvas element does not get along with percentage
* width and height so it ought to have pixel size specified.
*/
MAORI.general.resize = function() {
  MAORI.general.clientWH();
  //almost 100%
  MAORI.general.drawingCanvas.height = MAORI.general.clientHeight - 20 - 160;
  MAORI.general.drawingCanvas.width = (MAORI.general.clientWidth * 0.9) - 15 ;
}


/**
* Initialize MAORI client
*/
MAORI.general.init = function() {
  MAORI.general.drawingCanvas = document.getElementsByTagName('canvas')[0];
  //pass the canvas element to the EaselJS Stage instance
  //The Stage class abstracts away the Canvas element and
  //is the root level display container for display elements.
  MAORI.general.stage = new Stage(MAORI.general.drawingCanvas);
  MAORI.general.drawingCanvas.mousemove = MAORI.event.onMouseMove;
  MAORI.general.drawingCanvas.ondrop = MAORI.event.objectDropped;
  MAORI.general.drawingCanvas.ondragover = MAORI.event.cancelDefaultOperation;
  MAORI.general.drawingCanvas.ondragenter = MAORI.event.cancelDefaultOperation;
  MAORI.general.drawingCanvas.onmousedown = MAORI.event.onDragDrawable;
  MAORI.general.drawingCanvas.onmouseup = MAORI.event.onDropDrawable;
  MAORI.general.drawingCanvas.onmousemove = MAORI.event.onMouseMove;
  MAORI.general.drawingCanvas.onmouse
  window.onresize = MAORI.general.resize;
  /* Initialization code. */
  MAORI.draw.init();
  MAORI.event.init();
  MAORI.model.init();
  MAORI.tools.init('toolbox');
  MAORI.general.resize();
  document.addEventListener(MAORI.event.showTextInput,
                            MAORI.general.showTextInput, false);
};
