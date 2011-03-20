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
* @constructor
* @param {Array} array of objects.
*/
MAORI.BackForth = function(array) {
  var bfSize = array.length;
  if (!array instanceof Array || bfSize < 1) {
    throw 'Argument is not a valid Array';
  }
  var bfArray = array;
  var direction = 1;
  var currentIndex = 0;
  this.next = function() {
    if (currentIndex <= 0) {
      direction = 1;
    }else if (currentIndex >= bfSize - 1) {
      direction = -1;
    }
    currentIndex += direction;
    return bfArray[currentIndex];
  };
};
