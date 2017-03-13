-module(server_hello).
-export([
         init/2,
         content_types_accepted/2,
         content_types_provided/2,
         accept_post/2,
         allowed_methods/2
        ]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

accept_post(Req, State) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  PostContent = jiffy:decode(Body),
  io:format("~p~n", [PostContent]),
  {true, Req, State}.

content_types_accepted(Req, State) ->
  io:format("call:content_types_accepsted~n"),
  Result = [{{<<"application">>, <<"json">>, '*'}, 'accept_post'}],
  {Result, Req, State}.

content_types_provided(Req, State) ->
  io:format("call:provided~n"),
  Result = [{{<<"*">>, <<"*">>, '*'}, '_'}],
  {Result, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">> ], Req, State}.


-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").


-endif.
