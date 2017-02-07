-module(pomodoro_manager).

-export([
         %% Client APIs
         create_pomodoro_manager/0,
         start_pomodoro/2,
         cancel_pomodoro/1,
         skip_rest/1,
         %% OTP gen_server behaviors
         init/1,
         code_change/3,
         handle_call/3,
         handle_info/2,
         terminate/2,
         handle_cast/2
        ]).

-behavior(gen_server).

%% Clients APIs

create_pomodoro_manager() ->
  gen_server:start(
    pomodoro_manager,
    [#{}],
    []
   ).

start_pomodoro(UserID, Timers) ->
  gen_server:cast(pmodoro_manager, {cancel, Timers, UserID}),
  ok.

cancel_pomodoro(UserID) ->
  gen_server:cast(pmodoro_manager, {cancel, UserID}),
  ok.

skip_rest(UserID) ->
  gen_server:cast(pmodoro_manager, {cancel, UserID}),
  ok.

%% gen_server behaviors

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_info(_, _) ->
  ok.

handle_cast({start, Timers, UserID}, UserIDMap) ->
  Pids = start_timer(Timers),
  Map2 = maps:puts(UserID, #{timers => Timers, pids =>Pids}, UserIDMap),
  {noreply, Map2};
handle_cast({cancel, UserID}, UserIDMap) ->
  Pids = maps:get(UserID, UserIDMap, not_found),
  cancel_timer(Pids),
  Map2 = maps:remove(UserID, UserIDMap),
  {noreply, Map2};
handle_cast(Cast, State) ->
  io:format("~nUnexpected cast ~p is Called", [Cast]),
  {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init(State) ->
  {ok, State}.

terminate(_, _) ->
  ok.

%% Private methos

start_timer(Timers) ->
  lists:map(fun post_timer:start_timer/1, Timers).

cancel_timer(not_found) -> 
  ok;
cancel_timer(#{pids := Pids}) ->
  cancel_timer(Pids),
  ok;
cancel_timer([Head | Tail]) ->
  cancel_timer(Head),
  cancel_timer(Tail); 
cancel_timer([]) -> ok;
cancel_timer(Pid) -> Pid ! stop.

