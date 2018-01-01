-module(cowboy_timer_handler).
-export([
         init/2,
         content_types_accepted/2,
         allowed_methods/2,
         timeout/2,
         resource_exists/2,
         create_timer/2,
         delete_resource/2
        ]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"DELETE">>], Req, State}.


% handle post
content_types_accepted(Req = #{method := <<"POST">>}, State) ->
  Result = [{{<<"application">>, <<"json">>, '*'}, create_timer}],
  {Result, Req, State}.


delete_resource(Req, State) ->
  {valid, Hash} = hash_for(Req),
  Timers = ets:lookup(timers, Hash),
  lists:map(fun({_, Tref}) -> timer:cancel(Tref) end, Timers),
  ets:delete(timers, Hash),
  erlang:display({<<"DELETE : ">>,  Hash}),
  {true, Req, State}.

validate({valid, Hash}) ->
  erlang:display(Hash),
  Res = ets:lookup(timers, Hash),
  case lists:any(fun(_) -> true end, Res) of
    true ->
      {valid, Hash};
    false ->
      invalid
  end;
validate(_) ->
  invalid.

hash_for(#{bindings := Binds}) ->
  lists:foldl(
    fun (_, {valid, Hash}) -> {valid, Hash};
        ({hash, Hash}, _) -> {valid, Hash};
        (_, _) -> invalid
    end
    , invalid, Binds).


 
create_timer(Req, State) ->
  {ok, Data, _} = cowboy_req:read_body(Req),
  JsonData = jiffy:decode(Data, [return_maps]),

  Hocks = maps:get(<<"hocks">>, JsonData),


  Duration = maps:get(<<"duration">>, JsonData),
  HockUrls = lists:map(fun(#{<<"url">> := Url}) when is_binary(Url) ->
                           erlang:binary_to_list(Url)
                       end
                       ,Hocks),

  Hash = timer(Duration, HockUrls),

  NewReq = cowboy_req:set_resp_body(Hash, Req),

  {true, NewReq, State}.




resource_exists(Req = #{method := <<"POST">>}, State)->
  {false, Req, State};
resource_exists(Req = #{method := <<"DELETE">>}, State)->
  case validate(hash_for(Req)) of
    {valid, _} ->
      {true, Req, State};
    invalid ->
      {false, Req, State}
  end.

  
timer(Duration, Hocks) ->
  Hash = util:rand_hash(18),
  {ok, Tref} = timer:apply_after(Duration, cowboy_timer_handler, timeout, [Hocks, Hash]),
  ets:insert(timers, {Hash, Tref}),
  Hash.

timeout(Hocks, Hash) ->
  post_hocks(Hocks),
  ets:delete(timers, Hash),
  ok.

post_hocks([Hock | Hocks]) ->
  spawn(fun()->post(Hock)end),
  post_hocks(Hocks);
post_hocks([]) ->
  ok.

post(Url) ->
  Req = {Url, [], "text/plain", <<"ok">>},
  httpc:request(post, Req, [], []),
  erlang:display(Url),
  ok.


