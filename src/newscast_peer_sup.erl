-module(newscast_peer_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = #{id => newscast_peer,
             start => {newscast_peer, start_link, []},
             restart => temporary,
             shutdown => 5000
            },
    Flags = #{
      strategy => simple_one_for_one,
      period => 60,
      intensity => 10
     },
    {ok, {Flags, [Spec]}}.
