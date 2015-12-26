$(function() {
  $(".Debug-Draggable").draggable();

  $(".Debug-Droppable").droppable({
    drop: function(event, ui) {
      alert($(ui.draggable).html());
    }
  });

  $('.Action-OpenOverview').click(function() {
      $('.Overview').fadeIn();
  });

  $('.Action-ShowMapTest').click(function() {
	  //$('.MapTest').html('aaa');
      $('.MapTest2').html( $('.Arena').html() );
      $('.MapTest').fadeIn();
  });
  $('.Action-HideMapTest').click(function() {
     $('.MapTest2').fadeOut();
  });

  $('.Action-CloseOverview').click(function() {
     $('.Overview').fadeOut();
  });

  $('.Action-OpenDebug').click(function() {
      $('.DebugArea').fadeIn();
  });

  $('.Action-CloseDebug').click(function() {
     $('.DebugArea').fadeOut();
  });
  
  $('.Action-ShowDialog').click(function() {
    $( "#Dialog-Test" ).dialog({
      resizable: false,
      height: 'auto',
      modal: true,
      buttons: {
        Ok: function() {
          $( this ).dialog("close");
        },
        Cancel: function() {
          $( this ).dialog("close");
        }
      }
    });
  });

  $("#ZoomSlider").slider({
    range: "min",
    value: 100,
    min: 1,
    max: 100,
    slide: function(event, ui) {
      $("#Zoom").val(ui.value+"%");
      var zoom = ui.value/100;
      var $gameArea = $('.GameArea');
      $gameArea.css('zoom', zoom);
      $gameArea.css('-moz-transform', 'scale('+zoom+')');
      $gameArea.css('-o-transform', 'scale('+zoom+')');
    }
  });

  $(".Debug-DragCity").mousedown(function(e) {
	    var clone = $(this).clone();
	    console.log("town: %o", $(this));
	    console.log("clone: %o", clone);

	      clone.draggable({
	        cursor: 'move',
	        start: function(event, ui) {
	          ui.helper.css('transform', 'rotate(0deg) scale(1)');
	        },
	        stop: function(event, ui) {
	          ui.helper.css('transform', 'rotate(0deg) scale(1)');
	        }
	      });

	      clone.addClass("PlayerArea-CityItem");
	      clone.appendTo(".DragArea");
	      clone.trigger(e);

	      var posX = e.pageX - clone.width() / 2;
	      var posY = e.pageY - clone.height() / 2;
	      clone.css({position:"absolute", left:posX, top:posY});
	  });

	/*
	  $(".Eastward,.Westward").each(function () {
	    var height = $(this).height();
	    var width = $(this).width();
	    $(this).height(width);
	    $(this).width(height);
	  });
	*/

});