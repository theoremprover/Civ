function decodeData(encodedData){var tmp=document.createElement('div');tmp.innerHTML=encodedData;return(tmp.innerHTML)};$(function(){$(".Debug-Draggable").draggable();$(".Debug-Droppable").droppable({drop:function(event,ui){alert($(ui.draggable).html())}});$(".PlayerArea-CityItem").draggable();$(".Map-SquareContainer").droppable({accept:".PlayerArea-CityItem",drop:function(event,ui){var source=JSON.parse(decodeData($(ui.draggable).data("source"))),target=JSON.parse(decodeData($(this).data("target")));console.log(source);console.log(target);var action=([{source:source,target:target}]);sendAction(target)}})})