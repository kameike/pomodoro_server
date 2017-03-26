-module(cowboy_session_log).
-include("../post_data.hrl").
-export([
         init/2,
         to_html/2
        ]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

to_html(Req, State) ->
  UserID = cowboy_req:binding(user_id, Req),
  Pagination = #pagination{from = 1, count = 100},
  util:puts("req come"),
  #{result := Result} = session_log_server:read(#{user_id => UserID, pagination => Pagination}),
  {jiffy:encode(Result), Req, State}.

