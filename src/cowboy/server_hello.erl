-module(server_hello).
-export([
         init/2,
         to_html/2
        ]).

init(Req, State) ->
  {cowboy_rest, Req, State}.


to_html(Req, State) ->
  {"Hello! We are partly ready :D", Req, State}.

