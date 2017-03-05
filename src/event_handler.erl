-module(event_handler).

-export([
        execute/1 
        ]).

execute(#{type := Type, data := _Data}) when Type == conplete_event -> ok;
execute(#{type := Type, data := _Data}) when Type == conplete_event -> ok;
execute(#{type := Type, data := _Data}) when Type == conplete_event -> ok;
execute(#{type := Type, data := _Data}) when Type == conplete_event -> ok.
