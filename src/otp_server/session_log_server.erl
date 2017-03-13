-module(session_log_server).
-include("post_data.hrl").
-behavior(gen_server).

%% OTP gen_server behaviors
-export([
         init/1,
         code_change/3,
         handle_call/3,
         handle_info/2,
         terminate/2,
         handle_cast/2
        ]).


-export([
         create_session_log_server/0,
         load/1,
         save/1
        ]).

% Clients API
create_session_log_server() -> gen_server:start({global, session_log_server}, session_log_server, [#{}], []).
save(Data) -> gen_server:cast({global, session_log_server}, {log, Data}).
load(Data) -> gen_server:call({global, session_log_server}, {log, Data}).

% gen_server behaviors
init(State)-> {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(#{user_id := UserID, session_data := SessionData}, LogData) ->
  PrevUserLogList = user_log(maps:find(UserID, LogData)),
  NewUserLogList = lists:append([PrevUserLogList, [SessionData]]),
  {noreply, maps:puts(UserID, NewUserLogList, LogData)};
handle_cast(_, State) -> {noreply, State}.

handle_call({log, #{user_id := UserID, pagination := Pagination}}, _, LogData) -> 
  AllUesrLog = user_log(maps:find(UserID, LogData)),
  Value = #{ pagination => Pagination, result => sublist(AllUesrLog, Pagination) },
  {reply, Value, LogData};
handle_call(_, _, State) -> {reply, error, State}.

terminate(_, _) -> ok.
handle_info(_, _) -> ok.
% Private funcs

sublist(List, #pagination{from = From, count = Count}) ->
  case lists:flatlength(List) >= From of
    false -> [];
    true -> lists:sublist(List, From, Count) 
  end.

user_log({ok, List}) -> List;
user_log(error) -> [].

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

sublist_test() ->
  List = lists:seq(500, 600),

  Page1 = #pagination{from = 1, count = 30},
  Page2 = util:next_page(Page1),
  Page3 = util:next_page(Page2),
  Page4 = util:next_page(Page3),
  Page5 = util:next_page(Page4),
  ?assertEqual(sublist(List,Page1), lists:seq(500, 529)),
  ?assertEqual(sublist(List,Page2), lists:seq(530, 559)),
  ?assertEqual(sublist(List,Page3), lists:seq(560, 589)),
  ?assertEqual(sublist(List,Page4), lists:seq(590, 600)),
  ?assertEqual(sublist(List,Page5), []),
  ok.

-endif.

