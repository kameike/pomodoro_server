-module(cowboy_user).
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
  Method = cowboy_req:binding(method, Req),
  case Method of
    <<"create">> ->
      {ok, Result} = user_server:create_user(),
      Json = #{user_id => Result},
      NewReq = cowboy_req:set_resp_body(jiffy:encode(Json), Req),
      {true, NewReq, State};
    <<"all">> ->
      {ok, AllUser} = user_server:all_users(),
      NewReq = cowboy_req:set_resp_body(jiffy:encode(AllUser), Req),
      {true, NewReq, State};
    _ ->
      {false, Req, State}
  end.

content_types_accepted(Req, State) ->
  Result = [{'*', 'accept_post'}],
  {Result, Req, State}.

content_types_provided(Req, State) ->
  Result = [{{<<"*">>, <<"*">>, '*'}, '_'}],
  {Result, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">> ], Req, State}.

