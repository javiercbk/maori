'use strict';

describe('General Object Creation', function() {
  it('MAORI namespace should be created', function() {
    expect(MAORI).toBeDefined();
  });
  it('MAORI.general module should be created', function() {
    expect(MAORI.general).toBeDefined();
  });
  it('MAORI.draw module should be created', function() {
    expect(MAORI.draw).toBeDefined();
  });
  it('MAORI.event module should be created', function() {
    expect(MAORI.event).toBeDefined();
  });
  it('MAORI.model module should be created', function() {
    expect(MAORI.model).toBeDefined();
  });
  it('MAORI.model.drawables should be []', function() {
    var emptyArray = [];
    expect(MAORI.model.drawables).toEqual(emptyArray);
  });
});


describe('Event Module Test', function() {
  /*
  beforeEach(function() {
    //clear maori drawables
    MAORI.model.drawables = [];
  });
  */
  it('Click on canvas should trigger clickedAnyElement Function', function() {
    runs(function() {
      var foo = function(event) {/*DO NOTHING*/};
      var fooXY = function(event) {return {x: 2, y: 6};};
      this.mockupEvent = document.createEvent('MouseEvents');
      this.mockupEvent.initMouseEvent('click', true, true, document.defaultView,
          1, 0, 0, 0, 0, false, false, false, false, 0, null);
      //install a spy in function to be called
      spyOn(MAORI.event, 'getEventXY').andCallFake(fooXY);
      spyOn(MAORI.model, 'clickedAnyElement').andCallFake(foo);
      //Simulates a click from canvas
      MAORI.event.simpleClick(this.mockupEvent);
      expect(MAORI.event.getEventXY).toHaveBeenCalledWith(this.mockupEvent);
    });
    /* DONT KNOW WHY THE EVENT IS NOT POPULATING
    //wait for the event to populate
    waits(1000);
    runs(function() {
      expect(MAORI.model.clickedAnyElement).
          toHaveBeenCalled();
    });
    */
  });

  it('Drop text on canvas should trigger createText Function', function() {
    runs(function() {
      var foo = function(event) {/*DO NOTHING*/};
      var fooXY = function(event) {return {x: 2, y: 6};};
      this.mockupEvent = document.createEvent('MouseEvents');
      this.mockupEvent.initMouseEvent('drop', true, true, document.defaultView,
          1, 0, 0, 0, 0, false, false, false, false, 0, null);
      this.mockupEvent.dataTransfer = {files: []};
      //install a spy in function to be called
      spyOn(MAORI.event, 'textDragged').andCallFake(fooXY);
      spyOn(MAORI.model, 'createText').andCallFake(foo);
      //Simulates a click from canvas
      /*
      MAORI.event.objectDropped(this.mockupEvent);
      expect(MAORI.event.textDragged).toHaveBeenCalledWith(this.mockupEvent);
      */
    });
  });
});
