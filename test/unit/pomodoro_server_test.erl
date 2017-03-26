-module(pomodoro_server_test).
-include_lib("eunit/include/eunit.hrl").
-include("../../src/post_data.hrl").

server_test()->
  pomodoro_server:create_pomodoro_server(),
  pomodoro_server:start_pomodoro(#{
    timers => [
               #timer_seed{duration = 1000, done_events = #{}},
               #timer_seed{duration = 3000, done_events = #{}},
               #timer_seed{duration = 5000, done_events = #{}}
              ],
    user_id => id
   }),
  ok.

