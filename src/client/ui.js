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
* UI module
*/
MAORI.ui = {};


/**
* Shows the given element
* @param {Object} element to be shown.
* @param {Object} vector of Movement.
* @param {Integer} steps to take.
*/
MAORI.ui.moveElement = function(element, vector, steps) {
  var actualElement = document.getElementById(element);
  var elementShower = new MAORI.ui.ElementShower(actualElement, vector, steps);
  var shower = function() { elementShower.move(); };
  MAORI.ui.UIAnimation = setInterval(shower, 50);
};



/**
* @constructor
* Shows the given element in a set o steps
* @param {Element} element to be shown.
* @param {Object} vector of mevement.
* @param {Integer} steps to take.
*/
MAORI.ui.ElementShower = function(element, vector, steps) {
  this.e = element;
  var v = vector;
  var currentStep = 0;

  this.move = function() {
    if (currentStep >= steps) {
      clearInterval(MAORI.ui.UIAnimation);
      var button = this.e.getElementsByTagName('input')[0];
      if (button.value === '>') {
        button.value = '<';
      } else {
        button.value = '>';
      }
      var id = this.e.id;
      v.x = -v.x;
      v.y = -v.y;
      button.onclick = function(event) {
        MAORI.ui.moveElement(id, v, 10);
      }
      return;
    }
    this.applyVector();
    currentStep++;
  };

  this.applyVector = function() {
    var parsedTop = parseInt(this.e.style.top);
    var parsedLeft = parseInt(this.e.style.left);
    this.e.style.top = parsedTop + v.y + 'px';
    this.e.style.left = parsedLeft + v.x + 'px';
  }
};
