-module(cowboy_pomodoro).
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
  UserID = cowboy_req:binding(user_id, Req),
  Method = cowboy_req:binding(method, Req),
  UserExsit = user_server:user_exist(UserID),
  case {UserExsit, Method} of
    {not_found, _} ->
      NewReq = cowboy_req:set_resp_body(error_user_id_not_found(UserID), Req),
      {false, NewReq, State};
    {_, <<"start">>} -> 
      Timers = request_parser:create_timers(Req, UserID),
      pomodoro_server:start_pomodoro(#{timers => Timers, user_id => UserID}),
      {true, Req, State};
    {_, <<"cancel">>} ->
      {true, Req, State};
    {_, Method} -> 
      NewReq = cowboy_req:set_resp_body(error_unknown_method(Method), Req),
      {false, NewReq, State}
  end.

content_types_accepted(Req, State) ->
  Result = [{{<<"application">>, <<"json">>, '*'}, 'accept_post'}],
  {Result, Req, State}.

content_types_provided(Req, State) ->
  Result = [{{<<"*">>, <<"*">>, '*'}, '_'}],
  {Result, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">> ], Req, State}.

% private functions for error message

error_base() ->
  <<"[POMODORO_ERROR] ">>.

error_user_id_not_found(UserID) ->
  [
   error_base(),
   <<"User ID \"">>, UserID, <<"\" not found.\n">>
  ].

error_unknown_method(MethodName) ->
  [
   error_base(),
   MethodName,
   <<" not found.\n">>
  ].


