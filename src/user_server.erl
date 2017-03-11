-module(user_server).
-behavior(gen_server).

%% Client APIs
-export([
         start/0,
         create_user/0,
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
start() ->
  case gen_server:start_link({global, user_server}, user_server, #{}, []) of
    {ok, _} -> ok;
    ignore -> ok;
    {error, Error} -> throw(Error)
  end,
  ok.

create_user() ->
  gen_server:call({global, user_server}, {create_user}).
user_exist(UserID) ->
  gen_server:call({global, user_server}, {find, #{user_id => UserID}}).

%% gen_server behaviors

handle_call({create_user}, _From, State) ->
  UserID = util:rand_hash(30),
  NewState = maps:put(UserID, #{created => erlang:system_time(microsecond)}, State),
  {reply, {ok, UserID}, NewState};
handle_call({find, #{user_id := UserID}}, _From, State) ->
  Result = case maps:find(UserID, State) of
             {ok, Data} -> {ok, #{user_id=> UserID, data => Data}};
             error -> not_found
           end,
  {reply, Result, State}.

handle_info(_, _) -> ok.
handle_cast(_, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
init(State) -> {ok, State}.
terminate(_, _) -> ok.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

create_user_test() ->
  start(),
  {ok, Data} = create_user(),
  Result1 = user_exist(Data),
  Result2 = user_exist("not_exist"),
  ?assertMatch({ok, #{user_id := _, data := _}}, Result1),
  ?assertMatch(not_found, Result2),
  ok.

-endif.

