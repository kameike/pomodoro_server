-module(pomodoro_server).
-behavior(gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-include("post_data.hrl").
-endif.

%% Client APIs
-export([
         create_pomodoro_manager/0,
         start_pomodoro/2,
         cancel_pomodoro/1,
         skip_rest/1
        ]).

%% OTP gen_server behaviors
-export([
         init/1,
         code_change/3,
         handle_call/3,
         handle_info/2,
         terminate/2,
         handle_cast/2
        ]).

% Clients APIs
create_pomodoro_manager() -> gen_server:start(pomodoro_manager, [#{}], []).
start_pomodoro(UserID, Timers) -> gen_server:cast(pmodoro_manager, {cancel, Timers, UserID}).
cancel_pomodoro(UserID) -> gen_server:cast(pmodoro_manager, {cancel, UserID}).
skip_rest(UserID) -> gen_server:cast(pmodoro_manager, {cancel, UserID}).

%% gen_server behaviors
init(State) -> {ok, State}.
terminate(_, _) -> ok.

handle_call(_, _From, State) -> {reply, ok, State}.
handle_info(_, _) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({start, TimerSeeds, UserID}, UserIDMap) ->
  cancel_timer(maps:find(UserID, UserIDMap)),
  Pids = start_timers(TimerSeeds),
  Map2 = maps:put(UserID, Pids, UserIDMap),
  {noreply, Map2};
handle_cast({cancel, UserID}, UserIDMap) ->
  cancel_timer(maps:find(UserID, UserIDMap)),
  Map2 = maps:remove(UserID, UserIDMap),
  {noreply, Map2}.

%% Private methos
cancel_timer({ok, Timers}) -> lists:map(fun(Timer) -> timer:cancel_timer(Timer) end, Timers);
cancel_timer(error) -> ok.

start_timers(Timers) -> lists:map(fun(Timer)-> post_timer:start_timer(Timer) end, Timers).

-ifdef(EUNIT).

list_of_timer() ->
  [
   test_factory:dummy_timer(),
   test_factory:dummy_timer(),
   test_factory:dummy_timer()
  ].

list_of_dummy_pids() -> #{test_factory:user_id() => [ spawn(fun() -> ok end), spawn(fun() -> ok end), spawn(fun() -> ok end) ]}.

handle_start_cast_test() ->
  UserID = test_factory:user_id(),
  Result = handle_cast({start, list_of_timer(), UserID}, #{}),
  ?assertMatch({noreply, #{UserID := _}}, Result),

  {noreply, #{UserID := Pids}} = Result,
  ?assert(is_list(Pids)),

  lists:map( fun(Pid)-> ?assert(is_pid(Pid)) end, Pids),
  ok.

handle_cancel_cast_with_expected_user_id_test() ->
  UserID = test_factory:user_id(),
  Result = handle_cast({cancel, UserID}, list_of_dummy_pids()),
  {noreply, NewUserIdMap} = Result,
  ?assertEqual(0, maps:size(NewUserIdMap)),
  ok.

handle_cancel_cast_with_unexpected_user_id_test() ->
  {noreply, NewUserIDMap} = handle_cast({cancel, "unknown_user"}, #{}),
  ?assertEqual(0, maps:size(NewUserIDMap)).

-endif.

