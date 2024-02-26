$(document).ready(function() {
  var handwritingCanvas = {};

  Shiny.addCustomMessageHandler("initialize", function(id) {
     resizeCanvas();
    var canvas = document.getElementById(id + '-handwritingCanvas');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    handwritingCanvas[id] = new handwriting.Canvas(canvas, 3);

    // Enable undo and redo
    handwritingCanvas[id].set_Undo_Redo(true, true);
    console.log("initialize");
  });

  Shiny.addCustomMessageHandler("clearCanvas", function(id) {
    handwritingCanvas[id].erase();
    console.log("erase");
  });

  Shiny.addCustomMessageHandler("undoCanvas", function(id) {
    handwritingCanvas[id].undo();
    console.log("undo");
  });

  Shiny.addCustomMessageHandler("redoCanvas", function(id) {
    handwritingCanvas[id].redo();
    console.log("redo");
  });

  Shiny.addCustomMessageHandler("sendCanvas", function(id) {
    var trace = handwritingCanvas[id].trace;
    var options = {
      language: 'en',
      numOfReturn: 1
    };
    var callback = function(result, err) {
      if (err) {
        console.error(err);
      } else {
        Shiny.setInputValue(id + '-recognized_text', result[0]);
      }
    };
    handwriting.recognize(trace, options, callback);
    console.log("send");
  });

  // Resize all canvas to match their container size
    function resizeCanvas() {
      $("canvas").each(function() {
        var parent = this.parentElement;
        this.width = parent.clientWidth;
        this.height = parent.clientHeight;
      });
    }

    // Call resizeCanvas function when the window is resized
    $(window).on('resize', function() {
      resizeCanvas();
    });


  // Resize canvas to match its container size
  //function resizeCanvas(id) {
    //var canvas = document.getElementById(id + '-handwritingCanvas');
    //var parent = canvas.parentElement;
    //canvas.width = parent.clientWidth;
    //canvas.height = parent.clientHeight;
  //}

  // Call resizeCanvas function when the modal is shown
  //$(document).on('shown.bs.modal', function() {
    //resizeCanvas('canvasID');
  //});

  // Call resizeCanvas function when the window is resized
  //$(window).on('resize', function() {
    //resizeCanvas('canvasID');
  //});

  // Call resizeCanvas function initially to set the initial canvas size
  //resizeCanvas('canvasID');
});
