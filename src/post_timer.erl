-module(post_timer).
-include("post_data.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.






-export([
         cancel_post_timer/1,
         start_timer/1
        ]).

-export([
         finish/1
        ]).

start_timer(Timer) ->
  spawn(fun()-> start(Timer) end).

start(Timer) ->
  receive
    cancel ->
      ok
  after Timer#timer.duration ->
          handle(Timer#timer.done_events),
          ok
  end.

finish(_) ->
  ok.

cancel_post_timer(Pid) ->
  Pid ! cancel,
  ok.

handle(done_event) ->
  ok.

post(#post_data{url = Url, content = Content}) ->
  httpc:request(
    post, 
    { Url, [], "application/json", Content },
    [], []),
  ok.

-ifdef(EUNIT).

start_timer_test() ->
  Pid = start_timer(test_factory:dummy_timer()),
  ?assert(is_pid(Pid)).

-endif.

