-module(newscast).

-export([join/1]).

-type peer() :: {inet:ip_address(), inet:port()}.
-export_type([peer/0]).

join(Peer) ->
    newscast_correspondent:join(Peer).

