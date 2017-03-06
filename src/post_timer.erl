-module(post_timer).
-include("post_data.hrl").
-export([
         cancel_timer/1,
         start_timer/1
        ]).

%interface
start_timer(TimerSeed) -> spawn(fun()-> start_timer_from(TimerSeed) end).

cancel_timer(Pid) ->
  Pid ! cancel,
  ok.

%private
start_timer_from(#timer_seed{duration = Duration, done_events = DoneEvents}) -> 
  Timer = #timer{
             duration = Duration,
             done_events = DoneEvents,
             start_date_time = erlang:localtime()
            },
  start(Timer).

start(Timer) ->
  receive
    cancel ->
      ok
  after Timer#timer.duration ->
          handle_events(Timer#timer.done_events),
          ok
  end.

handle_events(DoneEvents) -> lists:foreach(fun(Event) -> event_handler:execute(Event) end, DoneEvents).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

start_timer_test() ->
  Pid = start_timer(test_factory:dummy_timer_seed()),
  ?assert(is_pid(Pid)).

-endif.

