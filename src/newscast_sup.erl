-module(newscast_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    IpAddr          = application:get_env(newscast, ip, {127,0,0,1}),
    Port            = application:get_env(newscast, port, 6000),
    MaxCacheSize    = application:get_env(newscast, cache_size, 20),
    RefreshInterval = application:get_env(newscast, refresh, 10000),
    Peer = {IpAddr, Port},
    ServiceSup = #{id => newscast_peer_service,
                   start => {newscast_peer_service, start_link, [IpAddr, Port, #{}]},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker},
    PeerSup = #{id => newscast_peer_sup,
                  start => {newscast_peer_sup, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => supervisor},
    Correspondent = #{id => newscast_correspondent,
                      start => {newscast_correspondent, start_link,
                                [Peer, MaxCacheSize, RefreshInterval]},
                      restart => permanent,
                      shutdown => 5000},
    Flags = #{
      strategy => one_for_all,
      period => 60,
      intensity => 10
     },
    {ok, {Flags, [PeerSup, ServiceSup, Correspondent]} }.
