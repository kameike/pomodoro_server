#!/bin/sh

rebar3 compile && rebar3 release && _build/default/rel/server/bin/server console  --name server_release@128.0.0.1
