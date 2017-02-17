-module(test_factory).
-include("post_data.hrl").

-export([
         dummy_timer/0,
         user_id/0
        ]).

dummy_timer() ->
  #timer{duration = 3600, post_data = <<"dummy_data">>}.

user_id() ->
  "hogehoge".



