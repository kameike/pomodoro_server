-module(util).
-export([
         puts/1,
         randHash/1,
         time/0
        ]).

-ifndef(EUNIT).
puts(Value) -> 
  io:format("~n~p", [Value]).
-endif.

-ifdef(EUNIT).
puts(Value) -> 
  io:format(user, "~p~n", [Value]).
-endif.

time() ->
  ok.


randHash(Size) ->
  Rand = crypto:strong_rand_bytes(Size),
  Base = base64:encode(Rand),
  WordSize = string:words(Base),
  string:sub_string(Base, 0, WordSize - 3).
  
  

