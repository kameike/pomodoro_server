-module(post_timer).
-include("post_data.hrl").

-export([
         cancel_post_timer/1,
         start_timer/1
        ]).

start_timer(Timer) ->
  spawn(fun() -> start(Timer) end).

start(Timer) ->
  util:puts("start timer!!"),
  receive
    {Ref, cancel} ->
      util:puts("canceled"),
      Ref ! {self(), ok}
  after Timer#timer.timing ->
          util:puts("timing!!"),
          post(Timer#timer.post_data)
  end.

cancel_post_timer(Pid) ->
  Pid ! {self(), cancel},
  ok.

post(#post_data{url = Url, content = Content}) ->
  httpc:request(
    post, 
    { Url, [], "application/json", Content },
    [], []),
  ok.

