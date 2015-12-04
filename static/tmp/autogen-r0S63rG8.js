function decodeData(encodedData){var tmp=document.createElement('div');tmp.innerHTML=encodedData;return(tmp.innerHTML)};$(function(){$(".Debug-Draggable").draggable();$(".Debug-Droppable").droppable({drop:function(event,ui){alert($(ui.draggable).html())}});$(".PlayerArea-CityItem").draggable();$(".Map-SquareContainer").droppable({accept:".PlayerArea-CityItem",drop:function(event,ui){var source=JSON.parse(decodeData($(ui.draggable).data("source"))),target=JSON.parse(decodeData($(this).data("target")));console.log(source);console.log(target);console.log(source.concat(target))}})})
function sendAction(action_str){sendAction_Fun(action_str,function(){})}
function sendAction_Fun(action_str,fun_after_response){xh=new XMLHttpRequest();xh.open("POST",'http://dev.civ:3000/command',true);xh.setRequestHeader("Content-type","application/json");xh.onreadystatechange=function(){if(xh.readyState==XMLHttpRequest.DONE)if(xh.status==200){var response=JSON.parse(xh.responseText);if(response.hasOwnProperty("Left")){alert(response["Left"])}else fun_after_response()}else{document.write(xh.responseText);document.close()}};xh.ontimeout=function(){alert("Timeout sending "+action_str)};xh.onerror=function(){alert("Error sending "+action_str)};xh.send(action_str)};xmlhttp=new XMLHttpRequest()
function longPoll(){xmlhttp.open("POST",'http://dev.civ:3000/game/Game%201',true);xmlhttp.timeout=1000*60*10;xmlhttp.setRequestHeader("Content-type","application/json");xmlhttp.onreadystatechange=function(){if(xmlhttp.readyState==XMLHttpRequest.DONE)if(xmlhttp.status==200){document.write(xmlhttp.responseText);document.close()}};xmlhttp.ontimeout=function(){longPoll()};xmlhttp.onerror=function(){longPoll()};xmlhttp.send("{\"tag\":\"GameGame\",\"contents\":\"Game 1\"}")}
function redirectTo(target_str){xmlhttp.open("GET",target_str,true);xmlhttp.onreadystatechange=function(){if(xmlhttp.readyState==XMLHttpRequest.DONE)if(xmlhttp.status==200){document.write(xmlhttp.responseText);document.close()}};xmlhttp.ontimeout=function(){alert("Timeout loading "+target_str)};xmlhttp.onerror=function(){alert("Error loading "+target_str)};xmlhttp.send(null)}
function sendAndRedirect(action_str,url){sendAction_Fun(action_str,function(){redirectTo(url)})}
function onUnload(){xmlhttp.abort()}