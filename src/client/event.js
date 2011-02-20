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
* event module 
* custom event init
*/
MAORI.event = {objectDropped: 'objectDropped',
	       clicked: 'clicked',
	       paint: 'paint',
	       repaint: 'repaint',
	       animate: 'animate',
	       stopAnimation: 'stopAnimation'};


/**
* Calculates the x and y position of an event
* @param {Object} ev event ocurred
* @returns {Object} point with the coordenates.
*/
MAORI.event.getEventXY = function(ev){
  var point = {x:0,y:0};
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
*/
MAORI.event.cancelDefaultOperation = function(event) {
  if (event.preventDefault) {
    event.preventDefault();
  }
  //Firefox
  if(event.stopPropagation){
    event.stopPropagation();
  }
};


/**
* Click event Handler
* @param {Object} event fired
*/
MAORI.event.simpleClick = function(event) {
  MAORI.event.fireEvent(MAORI.event.clicked, document, MAORI.event.getEventXY(event));
};


/**
* Key down event Handler
* @param {Object} event fired
*/
MAORI.event.keyDown = function(event){
  MAORI.event.cancelDefaultOperation(event);
  var keyCode = event.keyCode;
  if(keyCode == 17){ 
    MAORI.DOMObjs.ctrlPressed = true;
  }
};


/**
* mouse event Handler. the param is not class but
* @param {Class} ev eventRespose.
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
* @param {Object} target object to dispatch
* @param {Object} properties to add to the event
*/
MAORI.event.fireEvent = function (name, target, properties) {
  //Ready: create a generic event
  var evt = document.createEvent('Events') // or Event???
  evt.properties = properties;
  //Aim: initialize it to be the event we want
  evt.initEvent(name, true, true); //true for can bubble, true for cancelable
  target.dispatchEvent(evt);
};


/**
* Handler for drop event for the canvas.
* @param {Object} event fired.
*/
MAORI.event.objectDropped = function(event){
  MAORI.event.cancelDefaultOperation(event);
  var fileName = MAORI.event.uploadFile(event);
  var properties = {point:MAORI.event.getEventXY(event), file: fileName};
  MAORI.event.fireEvent(MAORI.event.objectDropped, document, properties);
};


/**
* Uploads a file.
* @returns the first file name (this is for now)
* TODO: define request module
*/
MAORI.event.uploadFile = function(event){
  var data = event.dataTransfer;
  var name = 'Could not be retrieved';
  if(data.files.length > 0){
    name = data.files[0].name;
  }else{
    name = data.getData('text');
  }
  return name;
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
    MAORI.DOMObjs.drawingCanvas.load("list.php?random=" +  (new Date).getTime());
  };
  */
};