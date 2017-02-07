-module(pomodoro_fem).

%% Client APIs
-export([
         create_pomodoro_manager/0,
         start_pomodoro/2,
         cancel_pomodoro/1,
         current_state/1,
         skip_rest/1
          ]).

%% fem_behaviors
-export([
         terminate/3,
         code_change/4,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         init/1
        ]).

%% fem_states
-export([
         rest/2,
         none/2,
         work/2
        ]).

-behavior(gen_fsm).

%% Clients APIs

create_pomodoro_manager() ->
  ok.
  
start_pomodoro(Pid, Timers) ->
  ok.

cancel_pomodoro(Pid) ->
  ok.

skip_rest(Pid) ->
  ok.

current_state(Pid) ->
  ok.

%% gen_fsm behaviors

none({start_work, SessionData}, {_, ParmData}) ->
  TempData = #{start_at => time, session_data => SessionData},
  {next_state, work, {TempData, ParmData}};
none(_, Data) ->
  sleep_self(Data).


work(cancel, Data) ->
  sleep_self(Data);
work(timeout, Data) ->
  {next_state, rest, Data}.

rest(skip, Data) ->
  sleep_self(Data).

terminate(_,_,_) ->
  ok.

init(Data) ->
  {ok, none, Data, hibernate}.

handle_info(_,_,_) ->
  ok.

handle_sync_event(_,_,_,_) ->
  ok.

handle_event(_,_,_) ->
  ok.

code_change(_,_,_,_) ->
  {ok, hoge, data}.

%% Private methos

sleep_self({_, ParmData}) ->
  {next_state, none, {null, ParmData}, hibernate}.
