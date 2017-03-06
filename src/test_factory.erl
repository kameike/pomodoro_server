-module(test_factory).
-include("post_data.hrl").

-export([
         dummy_timer_seed/0,
         user_id/0
        ]).

dummy_timer_seed() ->
  #timer_seed{
     duration = 3600,
     done_events = []
    }.

user_id() ->
  "hogehoge".



