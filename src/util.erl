-module(util).
-export([
         puts/1,
         rand_hash/1,
         time/0,
         decode_json/1
        ]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

puts(Value) -> io:format(user, "~p~n", [Value]).
time() -> ok.

decode_json(Data) ->
  JsonData = jiffy:decode(Data, [return_maps]),
  convert_binary_key_to_atom_key(JsonData).

convert_binary_key_to_atom_key(JsonData) when is_list(JsonData) == true -> lists:map(fun(Data)-> convert_binary_key_to_atom_key(Data) end, JsonData);
convert_binary_key_to_atom_key(JsonData) when is_map(JsonData) == false -> JsonData;
convert_binary_key_to_atom_key(JsonData) -> maps:fold(fun (Key, Value, AccIn) -> maps:put(erlang:binary_to_atom(Key, utf8), convert_binary_key_to_atom_key(Value), AccIn) end, #{}, JsonData).

rand_hash(Size) ->
  Rand = crypto:strong_rand_bytes(Size),
  Base = base64:encode(Rand),
  WordSize = string:words(Base),
  string:sub_string(Base, 0, WordSize - 3).

-ifdef(EUNIT).

decode_json_test() ->
  {ok, Bin} = file:read_file("test/data/binary_key_to_atom_key.json"),
  check_if_key_is_atom(decode_json(Bin)),
  ok.

check_if_key_is_atom(JsonData) when is_list(JsonData) == true -> lists:map(fun(Data) -> check_if_key_is_atom(Data) end, JsonData);
check_if_key_is_atom(JsonData) when is_map(JsonData) == true ->
  lists:foreach(fun(Data) -> ?assert(is_atom(Data)) end, maps:keys(JsonData)),
  lists:foreach(fun(Data) -> check_if_key_is_atom(Data) end, maps:values(JsonData));
check_if_key_is_atom(Data) -> ok.

-endif.

