-module(post_timer).
-include("post_data.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         cancel_post_timer/1,
         start_timer/3
        ]).

-export([
         finish/1
        ]).

start_timer(UserID, Duration, PostData) ->
  Timing = #timer{
              start_date_time = erlang:localtime(),
              post_data = PostData,
              duration = Duration,
              user_id = UserID
             },
  spawn(fun()-> start(Timing) end).

start(Timer) ->
  receive
    cancel ->
      ok
  after Timer#timer.duration ->
          post(Timer#timer.post_data)
  end.

finish(_) ->
  ok.

cancel_post_timer(Pid) ->
  Pid ! cancel,
  ok.

post(#post_data{url = Url, content = Content}) ->
  httpc:request(
    post, 
    { Url, [], "application/json", Content },
    [], []),
  ok.

-ifdef(EUNIT).

start_timer_test() ->
  Pid = start_timer(test_factory:user_id(), 1000, <<"">>),
  ?assert(is_pid(Pid)).

-endif.

