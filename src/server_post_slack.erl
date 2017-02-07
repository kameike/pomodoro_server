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
  Postables = create_timers_from(Req),
  SessionID = pomodoro_session:start_session(Postables),
  Json = {[{<<"sesion_id">>, SessionID}]},
  Req2 = cowboy_req:set_resp_body(jiffy:encode(Json), Req),
  {true, Req2, State}.

% pearse object to timer objecct

create_timers_from(Req) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  lists:map(fun create_timer/1, element(1, jiffy:decode(Body))).

create_timer({Label, Content}) ->
  #timer{timing = hock_timing(Label), post_data = parse_post_content(Content)};
create_timer(_) ->
  ok.

parse_post_content(PostTargetInfo) ->
  {[{<<"url">>, TargetUrl}, {<<"content">>, Content}]} = PostTargetInfo,
  #post_data{url = binary_to_list(TargetUrl), content = jiffy:encode(Content)}.

hock_timing(<<"start_work_hock">>) ->
  1000;
hock_timing(<<"start_rest_hock">>) ->
  1000 * 60 * 25;
hock_timing(<<"session_completion_hock">>) ->
  1000 * 60 * 30.

