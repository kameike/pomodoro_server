-module(user_server).
-behavior(gen_server).

%% Client APIs
-export([
         create_user_server/0,
         create_user/0,
         all_users/0,
         user_exist/1
        ]).

%% OTP gen_server behaviors
-export([
         init/1,
         code_change/3,
         handle_call/3,
         handle_info/2,
         terminate/2,
         handle_cast/2
        ]).

%% Client APIs
create_user_server() ->
  {ok, _} = gen_server:start({global, user_server}, user_server, #{}, []),
  ok.

create_user() ->
  gen_server:call({global, user_server}, create_user).

user_exist(UserID) ->
  gen_server:call({global, user_server}, {find, #{user_id => UserID}}).

all_users() ->
  gen_server:call({global, user_server}, all).

%% gen_server behaviors

handle_call(create_user, _From, State) ->
  UserID = util:rand_hash(30),
  NewState = maps:put(UserID, #{created => erlang:system_time(microsecond)}, State),
  {reply, {ok, UserID}, NewState};
handle_call({find, #{user_id := UserID}}, _From, State) ->
  Result = case maps:find(UserID, State) of
             {ok, Data} -> {ok, #{user_id=> UserID, data => Data}};
             error -> not_found
           end,
  {reply, Result, State};
handle_call(all, _From, State) ->
  {reply, {ok, maps:keys(State)}, State}.


handle_info(_, _) -> ok.
handle_cast(_, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
init(State) -> {ok, State}.
terminate(_, _) -> ok.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

create_user_test() ->
  {reply, {ok, UserID}, State} = handle_call({create_user}, pid, #{}),
  {ok, Updated} = maps:find(UserID,State),
  ?assertMatch(#{created :=  _}, Updated),
  ok.

find_user_test() ->
  UserID = util:rand_hash(40),
  State = #{ UserID => #{created => erlang:system_time(microsecond)}},
  {reply, Result, _} = handle_call( {find,#{user_id => UserID}}, pid, State),
  ?assertMatch({ok, #{user_id := _, data := #{created := _}}}, Result),
  ok.

-endif.

