FROM erlang:20

RUN git clone https://github.com/kameike/pomodoro_server home/timer-server \
      && cd home/timer-server \
      && rebar3 compile \
      && rebar3 release

EXPOSE 8080

CMD ["/home/timer-server/_build/default/rel/server/bin/server", "console"]
