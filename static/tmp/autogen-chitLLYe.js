$(function(){$(".Debug-Draggable").draggable();$(".Debug-Droppable").droppable({drop:function(event,ui){alert($(ui.draggable).html())}});$(".PlayerArea-CityItem").draggable();$(".Map-SquareContainer").droppable({accept:".PlayerArea-CityItem",drop:function(event,ui){var source=$(ui.draggable).data("source");console.log("source=%s",JSON.stringify(source));var target=$(this).data("target");console.log("target=%s",JSON.stringify(target));var move=[source,target];console.log("JSON.stringify(move)=%s",JSON.stringify(move));var found=0;for(i=0;i<allowedMoves.length;i++)if(JSON.stringify(allowedMoves[i])==JSON.stringify(move)){found=1;break};if(!found){alert("This move is not allowed!")}else{var action={"tag":"GameActionA","contents":[source,target]},actionstr=JSON.stringify(action);$("#Debug-Action").html(action);sendAction(actionstr)}}});$("#Debug-Send").click(function(){var action=$("textarea#Debug-Action").val();sendAction(action)});$(".Debug-DragCity").mousedown(function(e){var clone=$(this).clone();console.log("town: %o",$(this));console.log("clone: %o",clone);clone.draggable({cursor:'move',start:function(event,ui){ui.helper.css('transform','rotate(0deg) scale(1)')},stop:function(event,ui){ui.helper.css('transform','rotate(0deg) scale(1)')}});clone.addClass("PlayerArea-CityItem");clone.appendTo(".Debug");clone.trigger(e);var posX=e.pageX-clone.width()/2,posY=e.pageY-clone.height()/2;clone.css({position:"absolute",left:posX,top:posY})})})