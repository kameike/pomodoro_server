-module(server_post_slack).
-include("post_data.hrl").

-export([
         init/2,
         content_types_accepted/2,
         accept_post/2,
         allowed_methods/2
        ]).

% cowboy implement

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_accepted(Req, State) ->
  Result = [{{<<"application">>, <<"json">>, '*'}, 'accept_post'}],
  {Result, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">> ], Req, State}.

accept_post(Req, State) ->
  Timers = create_timers_from(Req),
  SessionID = 'nil',
  Json = {[{<<"sesion_id">>, SessionID}]},
  Req2 = cowboy_req:set_resp_body(jiffy:encode(Json), Req),
  {true, Req2, State}.

% pearse object to timer objecct

create_timers_from(Req) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  %% lists:map(fun create_timer/1, element(1, jiffy:decode(Body))).
  request_parser:create_timers(jiffy:decode(Body)).
