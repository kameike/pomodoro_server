-module(request_parser).
-include("post_data.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         create_timers/2
        ]).

%% constatnt
work_duration(error) -> 60 * 25; % defualt 5mim
work_duration({ok, Duration}) -> Duration.

rest_duration(error) -> 60 * 5; % defualt 25min
rest_duration({ok, Duration}) -> Duration.

%% interface
create_timers(Req, UserID) ->
  {ok, Data, _} = cowboy_req:read_body(Req),
  JsonData = util:decode_json(Data),
  [
   work_start_timer_seed_from_json(JsonData, UserID),
   work_done_timer_seed_from_json(JsonData, UserID),
   rest_done_timer_seed_from_json(JsonData, UserID)
  ].

%% privaate
work_start_timer_seed_from_json(JsonData, _UserID) ->
  #{event := #{ work_done := Events}} = JsonData,

  #timer_seed{
     duration = 0,
     done_events = timer_event_parser:parse_events(Events)
  }.

rest_done_timer_seed_from_json(JsonData, _UserID) ->
  #{event := #{ work_done := Events}} = JsonData,
  Property = get_property(maps:find(property, JsonData)),
  #{work_duration := WorkDuration, rest_duration := RestDuration} = Property,

  #timer_seed{
     duration = WorkDuration + RestDuration,
     done_events = timer_event_parser:parse_events(Events)
  }.

work_done_timer_seed_from_json(JsonData, UserID) ->
  CompleteSessionEvent = #{
    type => complete_session,
    data => #{ user_id => UserID }
   },

  #{event := #{ work_done := Events}} = JsonData,
  Property = get_property(maps:find(property, JsonData)),
  #{work_duration := Duration } = Property,

  #timer_seed{
     duration = Duration,
     done_events = timer_event_parser:parse_events(lists:append(Events, [CompleteSessionEvent]))
  }.

get_property({ok, Data}) when is_map(Data) ->
  WorkDuration = work_duration(maps:find(work_duration,Data)),
  RestDuration = rest_duration(maps:find(rest_duration,Data)),
  #{work_duration => WorkDuration, rest_duration => RestDuration}; 
get_property(error) -> get_property({ok, #{}}).

%% test
-ifdef(EUNIT).

json_with_propatry() ->
  {ok, Bin} = file:read_file("test/data/request_start_session.json"),
  util:decode_json(Bin).

json_without_propatry() ->
  {ok, Bin} = file:read_file("test/data/request_start_session_without_propaty.json"),
  util:decode_json(Bin).

stub_param() -> #{ user_id => "42" }.

work_start_with_prop_test() -> test_work_done_timer_seed(json_with_propatry(), 0, 1, fun(J,P) -> work_start_timer_seed_from_json(J,P)  end).
work_done_with_prop_test() -> test_work_done_timer_seed(json_with_propatry(), 3000, 2, fun(J,P) -> work_done_timer_seed_from_json(J,P)  end).
rest_done_with_prop_test() -> test_work_done_timer_seed(json_with_propatry(), 3500, 1, fun(J,P) -> rest_done_timer_seed_from_json(J,P)  end).

work_start_without_prop_test() -> test_work_done_timer_seed(json_without_propatry(), 0, 1, fun(J,P) -> work_start_timer_seed_from_json(J,P)  end).
work_done_without_prop_test() -> test_work_done_timer_seed(json_without_propatry(), 60 * 25, 2, fun(J,P) -> work_done_timer_seed_from_json(J,P)  end).
rest_done_without_prop_test() -> test_work_done_timer_seed(json_without_propatry(), 60 * 30, 1, fun(J,P) -> rest_done_timer_seed_from_json(J,P)  end).

test_work_done_timer_seed(JsonData, Duration, EventNum, Fun) ->
  #{event := Event} = JsonData,
  Result = Fun(JsonData, stub_param()),
  #timer_seed{ duration = ResultDuration, done_events = ResultDoneEvents } = Result,

  ?assertEqual(Duration, ResultDuration),
  ?assert(is_list(ResultDoneEvents)),
  ?assertEqual(lists:flatlength(ResultDoneEvents), EventNum),
  ok.

-endif.

