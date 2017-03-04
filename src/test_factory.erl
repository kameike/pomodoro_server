-module(test_factory).
-include("post_data.hrl").

-export([
         dummy_timer/0,
         user_id/0
        ]).

dummy_timer() ->
  #timer{
     start_date_time = erlang:localtime(),
     duration = 3600,
     done_events = []
    }.

user_id() ->
  "hogehoge".



