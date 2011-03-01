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
      this.mockupEvent = TEST.createClick(0, 0);
      //install a spy in function to be called
      spyOn(MAORI.event, 'getEventXY').andCallFake(fooXY);
      spyOn(MAORI.model, 'clickedAnyElement').andCallFake(foo);
      //Simulates a click from canvas
      MAORI.event.simpleClick(this.mockupEvent);
      expect(MAORI.event.getEventXY).toHaveBeenCalledWith(this.mockupEvent);
      expect(MAORI.model.clickedAnyElement).toHaveBeenCalledWith(this.mockupEvent);
    });
  });

  it('Drop text on canvas should trigger createText Function', function() {
    runs(function() {
      var foo = function(event) {/*DO NOTHING*/};
      var fooXY = function(event) {return {x: 2, y: 6};};
      this.mockupEvent = document.createEvent('MouseEvents');
      this.mockupEvent.initMouseEvent('drop', true, true, document.defaultView,
          1, 0, 0, 0, 0, false, false, false, false, 0, null);
      //this.mockupEvent.dataTransfer.setData('Test Text');
      //install a spy in function to be called
      spyOn(MAORI.event, 'textDragged').andCallFake(fooXY);
      spyOn(MAORI.model, 'createText').andCallFake(foo);
      //Simulates a click from canvas
      //MAORI.event.objectDropped(this.mockupEvent);
      //expect(MAORI.event.textDragged).toHaveBeenCalledWith(this.mockupEvent);
    });
  });
});

describe('Model Module Test', function() {
  
  beforeEach(function() {
    //clear maori model completely
    MAORI.model.drawables = [];
    MAORI.model.selected = [];
    MAORI.model.displaying = {x: 0, y: 0};
    MAORI.model.dragState = false;
    MAORI.model.dragDrawables = false;
  });
  
  it('on file creation it should create 2 drawable objects', function() {
    var mockupFile = {name: 'testFileName.test'};
    MAORI.model._createFile(50, 50, mockupFile);
    //1 File, 1 Raindrop
    expect(MAORI.model.drawables.length).toBe(2);
  });

  it('on text creation it should create 2 drawable objects', function() {
    var mockupText = 'testText';
    MAORI.model._createText(50, 50, mockupText);
    //1 text, 1 Raindrop
    expect(MAORI.model.drawables.length).toBe(2);
  });

  it('on file selection it should create 2 drawable objects', function() {
    runs(function() {
      //ensure click inside
      this.mockupEvent = TEST.createClick(55,55);
      var mockupFile = {name: 'testFileName.test'};
      MAORI.model._createFile(50, 50, mockupFile);
      MAORI.model.clickedAnyElement(this.mockupEvent);
      //1 Decorated File, 1 Raindrop,
      expect(MAORI.model.drawables.length).toBe(2);
      //grab decorated file
      expect(MAORI.model.drawables[0].decorator).toBeDefined();
    });
    //wait 2 seconds for animation to finish
    waits(2000);
    runs(function() {
      expect(MAORI.model.drawables.length).toBe(1);
      //click outside
      this.mockupEvent = TEST.createClick(1, 1);
      MAORI.model.clickedAnyElement(this.mockupEvent);
      expect(MAORI.model.drawables.length).toBe(1);
      expect(MAORI.model.drawables[0].decorator).toBeNull();
    });
  });

  it('on text selection it should create 2 drawable objects', function() {
    runs(function() {
      var mockupFile = 'testText';
      MAORI.model._createText(50, 50, mockupFile);
      this.mockupEvent = TEST.createClick(55, 55);
      MAORI.model.clickedAnyElement(this.mockupEvent);
      //1 text, 1 Raindrop
      expect(MAORI.model.drawables.length).toBe(2);
      expect(MAORI.model.drawables[0].decorator).toBeDefined();
    });
    //wait 2 seconds for animation to finish
    waits(2000);
  });
  runs(function() {
      expect(MAORI.model.drawables.length).toBe(1);
      this.mockupEvent = TEST.createClick(1, 1);
      MAORI.model.clickedAnyElement(this.mockupEvent);
      //1 text
      expect(MAORI.model.drawables.length).toBe(1);
      //decorator should be null because text was unselected
      expect(MAORI.model.drawables[0].decorator).toBeNull();
  });

  it('on file drag it should change the position of the file', function() {
    runs(function() {
      //ensure click inside
      var mockupEvent = TEST.createClick(55, 55);
      var mockupFile = {name: 'testFileName.test'};
      MAORI.model._createFile(50, 50, mockupFile);
      MAORI.model.clickedAnyElement(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50);
      expect(MAORI.model.drawables[0].y).toBe(50);
      //starts drag
      mockupEvent = TEST.createMouseDown(55, 55);
      MAORI.model.dragStart(mockupEvent);
      //generates 10 random movements
      var randomX = 0;
      var randomY = 0;
      for(var i = 0; i < 10; i++){
        randomX = Math.floor(Math.random()*110);
        randomY = Math.floor(Math.random()*110);
        mockupEvent = TEST.createMouseMove(randomX, randomY);
        MAORI.model.mouseMove(mockupEvent);
        expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 55));
        expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 55));
      }
      mockupEvent = TEST.createMouseUp(randomX, randomY);
      MAORI.model.dragStop(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 55));
      expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 55));
    });
  });

  it('on text drag it should change the position of the file', function() {
    runs(function() {
      //ensure click inside
      var mockupEvent = TEST.createClick(55, 55);
      var mockupText = 'testText';
      MAORI.model._createText(50, 50, mockupText);
      MAORI.model.clickedAnyElement(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50);
      expect(MAORI.model.drawables[0].y).toBe(50);
      //starts drag
      mockupEvent = TEST.createMouseDown(55, 55);
      MAORI.model.dragStart(mockupEvent);
      //generates 10 random movements
      var randomX = 0;
      var randomY = 0;
      for(var i = 0; i < 10; i++){
        randomX = Math.floor(Math.random()*110);
        randomY = Math.floor(Math.random()*110);
        mockupEvent = TEST.createMouseMove(randomX, randomY);
        MAORI.model.mouseMove(mockupEvent);
        expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 55));
        expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 55));
      }
      mockupEvent = TEST.createMouseUp(randomX, randomY);
      MAORI.model.dragStop(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 55));
      expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 55));
    });
  });

  it('on display drag it should change the position of the elements', function() {
    runs(function() {
      var mockupEvent = TEST.createClick(10, 15);
      //ensure click inside
      var mockupText = 'testText';
      MAORI.model._createText(50, 50, mockupText);
      MAORI.model._createFile(120, 40, mockupText);
      MAORI.model.clickedAnyElement(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50);
      expect(MAORI.model.drawables[0].y).toBe(50);
      //[File, Raindrop, Text, Raindrop] that's why
      //I use the third element
      expect(MAORI.model.drawables[2].x).toBe(120);
      expect(MAORI.model.drawables[2].y).toBe(40);
      //starts drag
      mockupEvent = TEST.createMouseDown(10, 15);
      MAORI.model.dragStart(mockupEvent);
      //generates 10 random movements
      var randomX = 0;
      var randomY = 0;
      for(var i = 0; i < 10; i++){
        randomX = Math.floor(Math.random()*110);
        randomY = Math.floor(Math.random()*110);
        mockupEvent = TEST.createMouseMove(randomX, randomY);
        MAORI.model.mouseMove(mockupEvent);
        expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 10));
        expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 15));
        expect(MAORI.model.drawables[2].x).toBe(120 + (randomX - 10));
        expect(MAORI.model.drawables[2].y).toBe(40 + (randomY - 15));
        expect(MAORI.model.displaying.x).toBe(randomX - 10);
        expect(MAORI.model.displaying.y).toBe(randomY - 15);
      }
      mockupEvent = TEST.createMouseUp(randomX, randomY);
      MAORI.model.dragStop(mockupEvent);
      expect(MAORI.model.drawables[0].x).toBe(50 + (randomX - 10));
      expect(MAORI.model.drawables[0].y).toBe(50 + (randomY - 15));
      expect(MAORI.model.drawables[2].x).toBe(120 + (randomX - 10));
      expect(MAORI.model.drawables[2].y).toBe(40 + (randomY - 15));
      expect(MAORI.model.displaying.x).toBe(randomX - 10);
      expect(MAORI.model.displaying.y).toBe(randomY - 15);
    });
  });
});
