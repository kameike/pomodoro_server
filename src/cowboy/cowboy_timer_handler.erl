-module(cowboy_timer_handler).
-export([
         init/2,
         content_types_accepted/2,
         allowed_methods/2,
         timeout/1,
         create_timer/2
        ]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_accepted(Req, State) ->
  Result = [{{<<"application">>, <<"json">>, '*'}, create_timer}],
  {Result, Req, State}.

create_timer(Req, State) ->
  {ok, Data, _} = cowboy_req:read_body(Req),
  JsonData = jiffy:decode(Data, [return_maps]),

  Hocks = maps:get(<<"hocks">>, JsonData),


  Duration = maps:get(<<"duration">>, JsonData),
  HockUrls = lists:map(fun(#{<<"url">> := Url}) when is_binary(Url) ->
                           erlang:binary_to_list(Url)
                       end
                       ,Hocks),

  erlang:display(Duration),
  erlang:display(HockUrls),
  timer(Duration, HockUrls),

  {true, Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

timer(Duration, Hocks) ->
  _Hash = util:rand_hash(88),
  Tref = timer:apply_after(Duration, cowboy_timer_handler, timeout, [Hocks]),
  erlang:display(Tref),
  ok.


timeout(Hash) ->
  post_hocks(Hash),
  erlang:display(Hash),
  erlang:display(self()).

post_hocks([Hock | Hocks]) ->
  spawn(fun()->post(Hock)end),
  post_hocks(Hocks);
post_hocks([]) ->
  ok.

post(Url) ->
  Req = {Url, [], "text/plain", <<"ok">>},
  httpc:request(post, Req, [], []),
  ok.



