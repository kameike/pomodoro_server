-module(user_server).

-behavior(gen_server).

-export([
         %% Client APIs
         pomodoro_manager_of/1,
         %% OTP gen_server behaviors
         init/1,
         code_change/3,
         handle_call/3,
         handle_info/2,
         terminate/2,
         handle_cast/2
        ]).

%% Client APIs

pomodoro_manager_of(User) ->
  ok.

%% gen_server behaviors

handle_call(_, _From, State) ->
  {reply, ok, State}.
 
handle_info(_, _) ->
  ok.

handle_cast(_, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init(State) ->
  {ok, State}.

terminate(_, _) ->
  ok.

