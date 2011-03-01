'use strict';


/**
* TEST namespace
*/
TEST = {};


/**
* Creates a test MouseEvent
* @param {Integer} x position of the event.
* @param {Integer} y position of the event.
* @param {String} type of the event.
* @return {Event} mockup event.
*/
TEST.createMouseEvent = function(testX, testY, type) {
  var mockupEvent = document.createEvent('MouseEvents');
  mockupEvent.initMouseEvent(type, true, true, document.defaultView,
          1, testX, testY, 0, 0, false, false, false, false, 0, null);
  mockupEvent.properties = {point: {x: testX, y: testY}};
  return mockupEvent;
};


/**
* Creates a test MouseDown event
* @param {Integer} x position of the event.
* @param {Integer} y position of the event.
* @return {Event} mouseDown event.
*/
TEST.createMouseDown = function(x, y) {
  return TEST.createMouseEvent(x, y, 'mouseDown');
};


/**
* Creates a test MouseUp event
* @param {Integer} x position of the event.
* @param {Integer} y position of the event.
* @return {Event} mouseUp event.
*/
TEST.createMouseUp = function(x, y) {
  return TEST.createMouseEvent(x, y, 'mouseUp');
};


/**
* Creates a test MouseMove event
* @param {Integer} x position of the event.
* @param {Integer} y position of the event.
* @return {Event} mouseMove event.
*/
TEST.createMouseMove = function(x, y) {
  return TEST.createMouseEvent(x, y, 'mouseMove');
};


/**
* Creates a test click event
* @param {Integer} x position of the event.
* @param {Integer} y position of the event.
* @return {Event} click event.
*/
TEST.createClick = function(x, y) {
  return TEST.createMouseEvent(x, y, 'click');
};