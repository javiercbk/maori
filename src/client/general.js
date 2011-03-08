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
* Initialize MAORI client
*/
MAORI.general.init = function() {
  MAORI.general.drawingCanvas = document.getElementsByTagName('canvas')[0];
  MAORI.general.drawingCanvas.mousemove = MAORI.event.onMouseMove;
  MAORI.general.drawingCanvas.ondrop = MAORI.event.objectDropped;
  MAORI.general.drawingCanvas.ondragover = MAORI.event.cancelDefaultOperation;
  MAORI.general.drawingCanvas.ondragenter = MAORI.event.cancelDefaultOperation;
  MAORI.general.drawingCanvas.onmousedown = MAORI.event.onDragDrawable;
  MAORI.general.drawingCanvas.onmouseup = MAORI.event.onDropDrawable;
  MAORI.general.drawingCanvas.onmousemove = MAORI.event.onMouseMove;
  MAORI.draw.init();
  MAORI.event.init();
  MAORI.model.init();
  MAORI.tools.init('toolbox');
  document.addEventListener(MAORI.event.showTextInput,
                            MAORI.general.showTextInput, false);
};
