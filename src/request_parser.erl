-module(request_parser).
-include("post_data.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         get_start_event/1,
         create_rest_timer_seed/1,
         create_work_timer_seed/1
        ]).


get_start_event(Req) ->
  JsonData = parse_req(Req),
  start_event_from_json(JsonData).

create_work_timer_seed(Req) ->
  JsonData = parse_req(Req),
  Param = get_req_param(Req),
  work_timer_seed_from_json(JsonData, Param).

create_rest_timer_seed(Req) ->
  JsonData = parse_req(Req),
  Param = get_req_param(Req),
  cancel_timer_seed_from_json(JsonData, Param).

%% constatnt

no_value() ->
  no_value.

work_duration(no_value) -> 1000 * 60 * 25; % defualt 5mim
work_duration(Duration) -> Duration.

rest_duration(no_value) -> 1000 * 60 * 5; % defualt 25min
rest_duration(Duration) -> Duration.

%% privaate

work_timer_seed_from_json(JsonData, #{user_id := UserID}) ->

  CompleteSessionEvent = create_work_compelete_event(UserID),

  #{<<"event">> := #{ <<"work_done">> := EventsData}} = JsonData,
  Events = parse_events(EventsData, EventsData),

  #{work_duration := Duration} = parse_property(JsonData),
  #timer_seed{
     duration = Duration,
     done_events = lists:append([CompleteSessionEvent], Events)
  }.

start_event_from_json(JsonData) ->
  ok.

cancel_timer_seed_from_json(JsonData, #{user_id := UserID}) ->
  ok.

parse_events(Events, Property) ->
  [hoge].

parse_property(JsonData) ->
  Property = maps:get(<<"property">>, JsonData, #{}),
  WorkDuration = maps:get(<<"work_duration">>, Property, no_value()),
  RestDuration = maps:get(<<"rest_duration">>, Property, no_value()),
  #{
    work_duration => work_duration(WorkDuration),
    rest_duration => rest_duration(RestDuration)
  }.


create_work_compelete_event(UserID) ->
  ok.

create_slack_post_event(Event) ->
  ok.

parse_req(Req) ->
  {ok, JsonData, _} = cowboy_req:read_body(Req),
  jiffy:decode(JsonData, [return_maps]).

get_req_param(Req) ->
  ok.

-ifdef(EUNIT).

stub_json() ->
  {ok, Bin} = file:read_file("test/data/request_start_session.json"),
  jiffy:decode(Bin, [return_maps]).

stub_param() ->
  #{
    user_id => "42" 
  }.

stub_post_slack() ->
  ok.

request_data() ->
  ok.

create_work_timer_seed_test() ->
  JsonData = stub_json(),
  #{<<"event">> := Event, <<"property">> := Property} = JsonData,

  Result = work_timer_seed_from_json(JsonData, stub_param()),
  #timer_seed{
     duration = ResultDuration,
     done_events = ResultDoneEvents
    } = Result,

  ?assertEqual(ResultDuration, 2250),
  ?assert(is_list(ResultDoneEvents)),
  ?assertEqual(lists:flatlength(ResultDoneEvents), 2),

  ok.

create_slack_post_event_test() ->
  ?assert(false),
  ok.

create_session_complete_event_test() ->
  ?assert(false),
  ok.
  

-endif.

