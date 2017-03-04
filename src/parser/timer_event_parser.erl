-module(timer_event_parser).
-export([parse_events/1]).

parse_events(Data) when is_list(Data) -> lists:map(fun(Elem) -> parse_event(Elem) end, Data).

parse_event(#{type := Type, data := Data}) when is_binary(Type) and is_map(Data) -> #{type => erlang:binary_to_atom(Type, utf8), data => Data};
parse_event(#{type := Type, data := Data}) when is_atom(Type) and is_map(Data) -> #{type => Type, data => Data}.

