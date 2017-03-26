-module(event_handler).
-include_lib("post_data.hrl").

-export([
        execute/2,
        check_type/1,
        type_error_message/1
        ]).

execute(#{type := complete_session, data := #{user_id := UserID}}, Timer) -> 
  SessionData = session_data_for(Timer),
  session_log_server:save(#{user_id => UserID, session_data => SessionData}),
  ok;
execute(#{type := post_slack, data := #{hock_path := Url, content := Content}}, _) -> 
  httpc:request(post, {"https://hooks.slack.com/services/" ++ binary_to_list(Url), [], "application/json", jiffy:encode(Content) }, [], []),
  ok;
execute(#{type := callback, data := #{function := Func}}, _) when is_function(Func) ->
  Func(),
  ok;
execute(_, _) -> ok.

check_type(#{type := complete_session, data := Data}) -> 
  case Data of
    #{user_id := UserID} when is_binary(UserID) -> ok;
    _ -> {error, no_user_id}
  end;
check_type(#{type := post_slack, data := Data}) ->
  case Data of
    #{url := Url} when is_binary(Url) -> ok;
    _ -> {error, no_url}
  end;
check_type(#{type := callback, data := #{function:= Func}}) when is_function(Func) -> ok;
check_type(_) -> {error, unknown_type}.

type_error_message(no_url) -> "no url for post post_slack event";
type_error_message(no_user_id) -> "user_id not found";
type_error_message(_) -> "internal server error (unrecognized tapple)".

session_data_for (Timer) ->
  #{
   start_time => Timer#timer.start_time,
   duration => Timer#timer.duration
  }.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

session_data_for_test() ->
  Timer = #timer{start_time = 0, duration = 100, done_events = []},
  ?assertMatch(#{start_time := 0, duration := 100}, session_data_for(Timer)),
  ok.

check_type_of_post_slack_test() ->
  ?assertMatch( ok, check_type(#{type => post_slack, data => #{url => <<"http://hgogehoge">>}})),
  ?assertMatch({error, no_url}, check_type(#{type => post_slack, data => no_data})).

check_type_of_complete_session_test() ->
  ?assertMatch(ok, check_type(#{type => complete_session, data => #{user_id => <<"useridhash">>}})),
  ?assertMatch({error, no_user_id}, check_type(#{type => complete_session, data => wrong_data})).

-endif.

