%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
  inets:start(),
  Dispatch = cowboy_router:compile([{host(), route_list()}]),
  {ok, _} = cowboy:start_clear(aip, 100,
                               config(),
                               #{env => #{dispatch => Dispatch}}
                              ),
  server_sup:start_link().

config() ->
  [{port, 8080}].

host() ->
  '_'. % much with any host

route_list() ->
  [
   {"/hello", server_hello, []},
   {"/slack_me", server_post_slack, []}
  ].


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
