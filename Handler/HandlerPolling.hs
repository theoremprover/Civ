module Handler.HandlerPolling where

import Import

import Text.Julius(rawJS)
import Data.Aeson

import Polls

noPollingJulius = toWidget [julius|
function longPoll() {}
|]

longPollingJulius target affected = toWidget [julius|

xmlhttp = new XMLHttpRequest();

function longPoll()
{
  xmlhttp.open("POST",'@{target}', true);
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
  xmlhttp.send(#{rawJS $ toJSONString $ affected});
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
|]

onclickHandler jsonobject = "sendAction(" ++ toJSONString jsonobject ++")"

toJSONString jsonobject = show $ encode jsonobject

sendJSONJulius = toWidget [julius|

function sendAction(action_str)
{
  sendAction_Fun(action_str,function() {});
}

function sendAction_Fun(action_str,fun_after_response)
{
  xh = new XMLHttpRequest();
  xh.open("POST",'@{CommandR}', true);
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
|]
