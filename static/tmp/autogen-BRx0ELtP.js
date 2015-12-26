$(function(){$(".PlayerArea-CityItem").draggable();$(".PlayerAction").mousedown(function(e){var strSource=JSON.stringify($(this).data("source")),strTarget='';console.log(strSource);for(i=0;i<allowedMoves.length;i++)if(JSON.stringify(allowedMoves[i][0])==strSource){strTarget=JSON.stringify(allowedMoves[i][1]);console.log(strTarget);$('.Map-SquareContainer').each(function(){if(JSON.stringify($(this).data("target"))==strTarget)$(this).addClass("PossibleSquare")})}});$(".PlayerAction").draggable({cursor:"move",helper:'clone',revert:true,stop:function(){console.log("stop - remove markers");$(".PossibleSquare").removeClass("PossibleSquare")},});$(".Map-SquareContainer").droppable({accept:".PlayerAction,.PlayerAreaCityItem",drop:function(event,ui){var source=$(ui.draggable).data("source"),target=$(this).data("target"),move=[source,target];console.log("source=%s",JSON.stringify(source));console.log("target=%s",JSON.stringify(target));console.log("JSON.stringify(move)=%s",JSON.stringify(move));var found=0;for(i=0;i<allowedMoves.length;i++)if(JSON.stringify(allowedMoves[i])==JSON.stringify(move)){found=1;break};if(!found){alert("This move is not allowed!")}else{ui.helper.fadeOut();var action={"tag":"GameActionA","contents":[source,target]},actionstr=JSON.stringify(action);sendAction(actionstr)}}})})
function sendAction(action_str){sendAction_Fun(action_str,function(){})}
function sendAction_Fun(action_str,fun_after_response){xh=new XMLHttpRequest();xh.open("POST",'http://dev.civ:3000/command',true);xh.setRequestHeader("Content-type","application/json");xh.onreadystatechange=function(){if(xh.readyState==XMLHttpRequest.DONE)if(xh.status==200){var response=JSON.parse(xh.responseText);if(response.hasOwnProperty("Left")){alert(response["Left"])}else fun_after_response()}else{document.write(xh.responseText);document.close()}};xh.ontimeout=function(){alert("Timeout sending "+action_str)};xh.onerror=function(){alert("Error sending "+action_str)};xh.send(action_str)};xmlhttp=new XMLHttpRequest()
function longPoll(){xmlhttp.open("POST",'http://dev.civ:3000/game/game2',true);xmlhttp.timeout=1000*60*10;xmlhttp.setRequestHeader("Content-type","application/json");xmlhttp.onreadystatechange=function(){if(xmlhttp.readyState==XMLHttpRequest.DONE)if(xmlhttp.status==200){document.write(xmlhttp.responseText);document.close()}};xmlhttp.ontimeout=function(){longPoll()};xmlhttp.onerror=function(){longPoll()};xmlhttp.send("{\"tag\":\"GameGame\",\"contents\":\"game2\"}")}
function redirectTo(target_str){xmlhttp.open("GET",target_str,true);xmlhttp.onreadystatechange=function(){if(xmlhttp.readyState==XMLHttpRequest.DONE)if(xmlhttp.status==200){document.write(xmlhttp.responseText);document.close()}};xmlhttp.ontimeout=function(){alert("Timeout loading "+target_str)};xmlhttp.onerror=function(){alert("Error loading "+target_str)};xmlhttp.send(null)}
function sendAndRedirect(action_str,url){sendAction_Fun(action_str,function(){redirectTo(url)})}
function onUnload(){xmlhttp.abort()};var allowedMoves=[[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":13,"xCoor":4}}],[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":14,"xCoor":4}}],[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":14,"xCoor":5}}],[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":12,"xCoor":6}}],[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":13,"xCoor":6}}],[{"tag":"FigureSource","contents":["Red","Wagon"]},{"tag":"SquareTarget","contents":{"yCoor":14,"xCoor":6}}]]