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
* Custom event init.
*/
MAORI.event = {fileDropped: 'fileDropped',
  textDropped: 'textDropped',
  clicked: 'clicked',
  paint: 'paint',
  repaint: 'repaint',
  animate: 'animate',
  stopAnimation: 'stopAnimation',
  dragStart: 'dragStart',
  dragStop: 'dragStop',
  mouseMove: 'mouseMove',
  enableKeys: 'enableKeys',
  disableKeys: 'disableKeys',
  showTextInput: 'showTextInput',
  relateSelected: 'relateSelected'};


/**
* Indicates if the mouse is pressed
*/
MAORI.event.mousePressedState = false;


/**
* Indicates if special operation was performed
*/
MAORI.event.specialPerformed = false;


/**
* Timeout clock used to know if the user
* is performing an special action
*/
MAORI.event.clockId = -1;


/**
* Indicates where the mouse was pressed
*/
MAORI.event.pressedOn = {x: 0, y: 0};


/**
* Calculates the x and y position of an event
* @param {Event} ev event ocurred.
* @return {Point} point with the coordenates.
*/
MAORI.event.getEventXY = function(ev) {
  var point = {x: 0, y: 0};
  var elementPosition = MAORI.general.getPosition('canvasZone');
  if (ev.layerX || ev.layerX == 0) { // Firefox
    point.x = ev.layerX - elementPosition.x;
    point.y = ev.layerY - elementPosition.y;
  } else if (ev.offsetX || ev.offsetX == 0) { // Opera
    point.x = ev.offsetX - elementPosition.x;
    point.y = ev.offsetY - elementPosition.y;
  }
  return point;
};


/**
* Prevents the default action on certain event like
* drag and drop.
* @param {Event} event fired.
*/
MAORI.event.cancelDefaultOperation = function(event) {
  if (event.preventDefault) {
    event.preventDefault();
  }
  //Firefox
  if (event.stopPropagation) {
    event.stopPropagation();
  }
};


/**
* Click event Handler
* @param {Event} event fired.
*/
MAORI.event.simpleClick = function(event) {
  MAORI.event.pressedOn.x = -1;
  MAORI.event.pressedOn.y = -1;
  var properties = {point: MAORI.event.getEventXY(event)};
  event.properties = properties;
  MAORI.model.clickedAnyElement(event);
};


/**
* Key down event Handler
* @param {Event} event fired on key down.
*/
MAORI.event.keyDown = function(event) {
  MAORI.event.cancelDefaultOperation(event);
  var keyCode = event.keyCode;
  if (keyCode == 17) {
    MAORI.DOMObjs.ctrlPressed = true;
  }
};


/**
* mouse event Handler.
* @param {Event} ev eventRespose.
* @return {Point} point where the event occurred.
*/
MAORI.event.ev_mousemove = function(ev) {
  // Get the mouse position relative to the canvas element.
  var point = MAORI.event.getEventXY(ev);
  return point;
};


/**
* Onclick canvas function
* @param {Event} event click performed.
*/
MAORI.event.onClickCanvas = MAORI.event.simpleClick;


/**
* Fires a given event
* @param {String} name of the event.
* @param {Object} target object to dispatch.
* @param {Object} properties to add to the event.
*/
MAORI.event.fireEvent = function(name, target, properties) {
  //Ready: create a generic event
  var evt = document.createEvent('Events');
  evt.properties = properties;
  //Aim: initialize it to be the event we want
  evt.initEvent(name, true, true); //true for can bubble, true for cancelable
  target.dispatchEvent(evt);
};


/**
* Handler for drop event for the canvas.
* @param {Event} event fired.
*/
MAORI.event.objectDropped = function(event) {
  MAORI.event.cancelDefaultOperation(event);
  var data = event.dataTransfer;
  if (data.files.length > 0) {
    //file dragged
    MAORI.event.fileDragged(event);
  } else {
    //text dragged
    MAORI.event.textDragged(event);
  }
};


/**
* Fires an event when text is dragged into the canvas.
* @param {Event} event onDrop.
*/
MAORI.event.textDragged = function(event) {
  var propertyToAdd = {point: MAORI.event.getEventXY(event)};
  event.properties = propertyToAdd;
  MAORI.model.createText(event);
};


/**
* Uploads a file.
* @param {Event} event onDrop.
*/
//TODO: define request module
MAORI.event.fileDragged = function(event) {
  var data = event.dataTransfer;
  var name = 'Could not be retrieved';
  //for each file fires an event
  for (var i = 0; i < data.files.length; i++) {
    var propertyToAdd = {file: data.files[i],
                          point: MAORI.event.getEventXY(event)};
    //hack: only to avoid putting all in the same pixel
    propertyToAdd.point.x += 30 * i;
    event.properties = propertyToAdd;
    MAORI.model.createFile(event);
  }
  /*
  var data = event.dataTransfer;
  var boundary = '------multipartformboundary' + (new Date).getTime();
  var dashdash = '--';
  var crlf     = '\r\n';

  // Build RFC2388 string.
  var builder = '';
  builder += dashdash;
  builder += boundary;
  builder += crlf;

  //Por ahora no voy a necesitar este objeto
  //var xhr = new XMLHttpRequest();

  // For each dropped file.
  for (var i = 0; i < data.files.length; i++) {
    var file = data.files[i];

    // Generate headers.
    builder += 'Content-Disposition: form-data; name="user_file[]"';
    if (file.fileName) {
      builder += '; filename="' + file.fileName + '"';
    }

    builder += crlf;

    builder += 'Content-Type: application/octet-stream';
    builder += crlf;
    builder += crlf;

    //porahora no necesito esto
    //Append binary data.
    /*
    builder += file.getAsBinary();
    builder += crlf;

    // Write boundary.
    builder += dashdash;
    builder += boundary;
    builder += crlf;
  }

  // Mark end of the request.
  builder += dashdash;
  builder += boundary;
  builder += dashdash;
  builder += crlf;

  //arma el post, por ahora no lo hago
  xhr.open("POST", "upload.php", true);
  xhr.setRequestHeader('content-type', 'multipart/form-data; boundary='
                       + boundary);
  xhr.sendAsBinary(builder);

  xhr.onload = function(event) {
    // If we got an error display it.
    if (xhr.responseText) {
      alert(xhr.responseText);
    }
    MAORI.DOMObjs.drawingCanvas.load("list.php?random="
+  (new Date).getTime());
  };
  */
};


/**
* Called when drag started and fires a dragStarted event
* to start a clock and verifies if user is performing a
* special action.
* @param {Event} event onMouseDown.
*/
MAORI.event.onDragDrawable = function(event) {
  MAORI.event.cancelDefaultOperation(event);
  MAORI.event.mousePressedState = true;
  var propertyToAdd = {point: MAORI.event.getEventXY(event)};
  MAORI.event.pressedOn.x = propertyToAdd.point.x;
  MAORI.event.pressedOn.y = propertyToAdd.point.y;
  event.properties = propertyToAdd;
  event.properties.callback = new MAORI.event.specialOperationCallback(event);
  MAORI.event.fireEvent(MAORI.event.dragStart, document, event.properties);
  MAORI.model.dragStart(event);
};


/**
* Called when drag stopped
* @param {Event} event onMouseUp.
*/
MAORI.event.onDropDrawable = function(event) {
  if (!MAORI.event.mousePressedState) {
    //FIXME: this is a hack to determine
    //if special action was executed.
    //it should remove the event temporary
    return;
  }
  MAORI.event.cancelDefaultOperation(event);
  //forces timeout clear
  clearTimeout(MAORI.event.clockId);
  var propertyToAdd = {point: MAORI.event.getEventXY(event)};
  MAORI.event.mousePressedState = false;
  //if special operation was performed
  //avoid method being called
  if (MAORI.event.specialPerformed) {
    MAORI.event.specialPerformed = false;
    return;
  }
  if (MAORI.event.pressedOn.x === propertyToAdd.point.x &&
      MAORI.event.pressedOn.y === propertyToAdd.point.y) {
    //it was a click
    MAORI.event.simpleClick(event);
    return;
  }
  MAORI.event.pressedOn.x = -1;
  MAORI.event.pressedOn.y = -1;
  event.properties = propertyToAdd;
  MAORI.model.dragStop(event);
};


/**
* Track mouse movement and decides wether the
* event is meaningfull or not. If it is fires
* an mouseMove event.
* @param {Event} event onMouseOver.
*/
MAORI.event.onMouseMove = function(event) {
  MAORI.event.cancelDefaultOperation(event);
  //forces timeout clear
  clearTimeout(MAORI.event.clockId);
  if (MAORI.event.mousePressedState) {
    var propertyToAdd = {point: MAORI.event.getEventXY(event)};
    event.properties = propertyToAdd;
    MAORI.model.mouseMove(event);
  }
};


/**
* Starts a timer in order to check for edition,
* if timeot suceeds it call its  callback.
* @param {Event} event in this case dragStart.
*/
MAORI.event.checkForEdition = function(event) {
  var callbackPerform = function() {
    event.properties.callback.performCallback();
  };
  //2 seconds
  MAORI.event.clockId = setTimeout(callbackPerform, 2000);
};


/**
* Callback special operation over a xy coordenate
* @param {Event} event to be appended.
* @this
*/
MAORI.event.specialOperationCallback = function(event) {
  this.event = event;

  this.performCallback = function() {
    MAORI.event.specialPerformed = true;
    MAORI.model.specialOperation(this.event);
  };
};


/**
* Executed when onKeyUp event is fired.
* @param {Event} event KeyUp.
*/
MAORI.event.onKeyUp = function(event) {
  var unicode = event.keyCode ? event.keyCode : event.charCode;
  var actualKey = String.fromCharCode(unicode);
  //console.log('KeyUpEvent -> key pressed: '+ actualKey);
};


/**
* Executed when onKeyDown event is fired.
* @param {Event} event KeyDown.
*/
MAORI.event.onKeyDown = function(event) {
  var unicode = event.keyCode ? event.keyCode : event.charCode;
  var actualKey = String.fromCharCode(unicode);
  if (event.altKey) {
    actualKey = 'alt + ' + actualKey;
  }
  if (event.ctrlKey) {
    actualKey = 'ctrl + ' + actualKey;
  }
  if (event.shiftKey) {
    actualKey = 'shift + ' + actualKey;
  }
  //Enter Key
  if (unicode = 13) {

  }
  //console.log('KeyDownEvent -> key pressed: '
  //+ actualKey + ' unicode: ' + unicode);
};


/**
* Enables Key event handling
*/
MAORI.event.enableKeyEvent = function() {
  document.onkeyup = MAORI.event.onKeyUp;
  document.onkeydown = MAORI.event.onKeyDown;
};


/**
* Disables Key event handling
*/
MAORI.event.disableKeyEvent = function() {
  document.onkeyup = null;
  document.onkeydown = null;
};


/**
* Event module initializer
*/
MAORI.event.init = function() {
  document.addEventListener(MAORI.event.dragStart,
                            MAORI.event.checkForEdition, false);
  document.addEventListener(MAORI.event.disableKeys,
                            MAORI.event.disableKeyEvent, false);
  document.addEventListener(MAORI.event.enableKeys,
                            MAORI.event.enableKeyEvent, false);
  //By default Key Events are enabled
  MAORI.event.enableKeyEvent();
};
