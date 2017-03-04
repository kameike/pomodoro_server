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
  request_parser:create_timers(Req),
  {true, Req, State}.
