toggleDebugArea = function() {
  alert('Debug');
}

$(function() {
    $('#activator').click(function(){
        $('#overlay').fadeIn('fast',function(){
            $('#box').animate({'top':'160px'},500);
        });
    });
    $('#boxclose').click(function(){
        $('#box').animate({'top':'-200px'},500,function(){
            $('#overlay').fadeOut('fast');
        });
    });

});

    $( "#dialog-confirm" ).dialog({
      resizable: false,
      height:140,
      modal: true,
      buttons: {
        "Delete all items": function() {
          $( this ).dialog( "close" );
        },
        Cancel: function() {
          $( this ).dialog( "close" );
        }
      }
    });
  });

$(function() {
  $(".Debug-Draggable").draggable();

  $(".Debug-Droppable").droppable({
    drop: function(event, ui) {
      alert($(ui.draggable).html());
    }
  });

  $(".PlayerArea-CityItem").draggable();

  $(".Map-SquareContainer").droppable({
    accept: ".PlayerArea-CityItem",
    drop: function(event, ui) {
      var source = $(ui.draggable).data("source");
      console.log("source=%s", JSON.stringify(source));

      var target = $(this).data("target");
      console.log("target=%s", JSON.stringify(target));

      var move = [source,target];
//      console.log("move.toSource()=%s",move.toSource());
      console.log("JSON.stringify(move)=%s",JSON.stringify(move));
      var found=0;
      for(i=0;i<allowedMoves.length;i++)
      {
        if(JSON.stringify(allowedMoves[i])==JSON.stringify(move)) { found=1; break; }
      }
      if(!found)
      {
        alert("This move is not allowed!");
      }
      else
      {
        var action = {"tag":"GameActionA","contents":[source,target]};
        var actionstr = JSON.stringify(action);

       $("#Debug-Action").html(action);
        sendAction(actionstr);
      }
    }
  });

  $("#Debug-Send").click(function() {
    var action = $("textarea#Debug-Action").val();
    sendAction(action);
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
      clone.appendTo(".Debug");
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


function sendAction(action_str)
{
  sendAction_Fun(action_str,function() {});
}

function sendAction_Fun(action_str,fun_after_response)
{
  xh = new XMLHttpRequest();
  xh.open("POST",'http://dev.civ:3000/command', true);
  xh.setRequestHeader("Content-type","application/json");
  xh.onreadystatechange = function() {
    if (xh.readyState == XMLHttpRequest.DONE)
    {
      if(xh.status == 200)
      {
        var response = JSON.parse(xh.responseText);
        if(response.hasOwnProperty("Left"))
        {
          alert(response["Left"]);
        }
        else
        {
          fun_after_response();
        }
      }
      else
      {
        document.write(xh.responseText);
        document.close();
      }
    }
  }
  xh.ontimeout = function() { alert("Timeout sending " + action_str); }
  xh.onerror = function() { alert("Error sending " + action_str); }
  xh.send(action_str);
}


xmlhttp = new XMLHttpRequest();

function longPoll()
{
  xmlhttp.open("POST",'http://dev.civ:3000/', true);
  xmlhttp.timeout = 1000*60*10;
  xmlhttp.setRequestHeader("Content-type","application/json");
  xmlhttp.onreadystatechange = function() {
    if (xmlhttp.readyState == XMLHttpRequest.DONE)
    {
      if(xmlhttp.status == 200)
      {
        document.write(xmlhttp.responseText);
        document.close();
      }
    }
  }
  xmlhttp.ontimeout = function() { longPoll(); }
  xmlhttp.onerror = function() { longPoll(); }
  xmlhttp.send("{\"tag\":\"GameAdmin\",\"contents\":[]}");
}

function redirectTo(target_str)
{
  xmlhttp.open("GET",target_str, true);
  xmlhttp.onreadystatechange = function() {
    if(xmlhttp.readyState == XMLHttpRequest.DONE)
    {
      if(xmlhttp.status == 200)
      {
        document.write(xmlhttp.responseText);
        document.close();
      }
    }
  }
  xmlhttp.ontimeout = function() { alert("Timeout loading " + target_str); }
  xmlhttp.onerror = function() { alert("Error loading " + target_str); }
  xmlhttp.send(null);
}

function sendAndRedirect(action_str,url)
{
  sendAction_Fun(action_str,
    function() { redirectTo(url); });
}

function onUnload()
{
  xmlhttp.abort();
}


function createGame()
{
  var gamename = document.getElementById("newgamename").value; 
  var cga = {"tag":"CreateGameA","contents":gamename};
  sendAction(JSON.stringify(cga));
}

function playGame(gamename_str,url)
{
  sendAndRedirect(JSON.stringify(
    { "tag":"SetSessionGamePlayerA",
      "contents":[
        gamename_str,
        document.getElementById(gamename_str+"_playername").value ]}),
    url);
}
