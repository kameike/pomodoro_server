-module(util).
-export([
         puts/1,
         randHash/1,
         time/0
        ]).

puts(Value) -> 
  io:format("~n~p", [Value]).

time() ->
  ok.


randHash(Size) ->
  Rand = crypto:strong_rand_bytes(Size),
  Base = base64:encode(Rand),
  WordSize = string:words(Base),
  string:sub_string(Base, 0, WordSize - 3).
  
  

