-module(post_timer).
-include("post_data.hrl").
-export([
         cancel_timer/1,
         remaining_time/1,
         start_timer/1
        ]).

%interface
start_timer(TimerSeed) -> spawn(fun()->start_timer_from(TimerSeed) end).

cancel_timer(Pid) ->
  Pid ! cancel,
  ok.

remaining_time(Pid) ->
  Pid ! {remaining_time, self()},
  receive
    {ok, Time} -> Time
  end.

%private
start_timer_from(#timer_seed{duration = Duration, done_events = DoneEvents}) -> 
  start(#timer{
           duration = Duration,
           done_events = DoneEvents,
           start_time = erlang:system_time(microsecond)
          }).

start(Timer) ->
  receive
    {remaining_time, Pid} -> 
      Pid ! {ok, get_wait_time(Timer)},
      start(Timer);
    cancel -> 
      ok
  after get_wait_time(Timer) ->
          handle_events(Timer#timer.done_events),
          ok
  end.

get_wait_time(Timer) ->
  case Timer#timer.duration + Timer#timer.start_time - erlang:system_time(microsecond) of
    Duration when Duration > 0 -> Duration;
    _ -> 0
  end.

handle_events(DoneEvents) -> lists:foreach(fun(Event) -> event_handler:execute(Event) end, DoneEvents).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_timer_test() ->
  Pid = start_timer(test_factory:dummy_timer_seed()),
  ?assert(is_pid(Pid)).

-endif.

