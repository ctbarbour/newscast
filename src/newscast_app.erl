-module(newscast_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    newscast_sup:start_link().

stop(_State) ->
    ok.

