-module(newscast_correspondent).

-behavior(gen_server).

-export([start_link/3]).
-export([register/2]).
-export([gossip/1]).
-export([join/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          peer             :: peer(),
          clock = 0        :: non_neg_integer(),
          refresh_interval :: pos_integer(),
          refresh_timer    :: undefined | reference(),
          refresh_ref      :: reference(),
          agents = []      :: [{pid(), newscast_agent:id()}],
          cache            :: newscast_cache:cache(),
          peer_ref         :: undefined | {pid(), undefined | reference()}
         }).

-type peer() :: {inet:ip_address(), inet:port_number()}.

start_link(Peer, MaxCacheSize, RefreshInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Peer, MaxCacheSize, RefreshInterval], []).

register(AgentId, Agent) ->
    gen_server:call(?MODULE, {register, AgentId, Agent}).

gossip(Payload) ->
    gen_server:call(?MODULE, {gossip, Payload}).

join(Peer) ->
    gen_server:cast(?MODULE, {join, Peer}).

init([Peer, MaxCacheSize, RefreshInterval]) ->
    Ref = make_ref(),
    self() ! {refresh, Ref},
    State = #state{
               peer = Peer,
               cache = newscast_cache:new(MaxCacheSize),
               refresh_ref = Ref,
               refresh_interval = RefreshInterval},
    {ok, State}.

handle_call({gossip, {RemoteTimestamp, RemoteCache}}, _From, State) ->
    Cache = handle_gossip(RemoteTimestamp, RemoteCache, State),
    {reply, State#state.cache, State#state{cache = Cache}};
handle_call({register, AgentId, Agent}, _From, State) ->
    case lists:keyfind(AgentId, 2, State#state.agents) of
        {AgentId, OtherAgent} ->
            {reply, {error, {already_exists, {AgentId, OtherAgent}}}, State};
        {AgentId, Agent} ->
            {reply, ok, State};
        false ->
            erlang:monitor(process, Agent),
            {reply, ok, State#state{agents = [{Agent, AgentId} | State#state.agents]}}
    end;
handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({join, Peer}, State) ->
    erlang:cancel_timer(State#state.refresh_timer),
    {noreply, handle_cycle([Peer], State)};
handle_cast({register, AgentId, Agent}, State) ->
    _MRef = monitor(process, Agent),
    {noreply, State#state{agents = [{Agent, AgentId} | State#state.agents]}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({refresh, Ref}, #state{refresh_ref = Ref} = State) ->
    Peers = [Peer || Peer <- newscast_cache:peers(State#state.cache),
                     Peer /= State#state.peer],
    {noreply, handle_cycle(Peers, State)};
handle_info({'DOWN', _MRef, process, Agent, _Reason}, State) ->
    case lists:keytake(Agent, 1, State#state.agents) of
        {value, {Agent, _Id}, Agents} ->
            {noreply, State#state{agents = Agents}};
        false ->
            {noreply, State}
    end;
handle_info({newscast_peer, {up, Pid}}, #state{peer_ref = {Pid, _}} = State) ->
    Ref = newscast_peer:gossip(Pid, {State#state.clock, State#state.cache}),
    {noreply, State#state{peer_ref = setelement(2, State#state.peer_ref, Ref)}};
handle_info({newscast_peer, {response, Pid, Ref, {RemoteTimestamp, RemoteCache}}},
            #state{peer_ref = {Pid, Ref}} = State) ->
    ok = newscast_peer:close(Pid),
    Cache = handle_gossip(RemoteTimestamp, RemoteCache, State),
    {noreply, State#state{cache = Cache, peer_ref = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_cycle(Peers, State) ->
    Fold = fun({Agent, _Id}, Ns) -> [newscast_agent:get_news(Agent) | Ns] end,
    News = lists:foldl(Fold, [], State#state.agents),
    Cache = newscast_cache:insert(State#state.peer, State#state.clock, News, State#state.cache),
    NewState = gossip_cache(Peers, State#state{cache = Cache}),
    Ref = make_ref(),
    TRef = erlang:send_after(State#state.refresh_interval, self(), {refresh, Ref}),
    NewState#state{refresh_timer = TRef, clock = State#state.clock + 1, refresh_ref = Ref}.

gossip_cache([], State) ->
    State;
gossip_cache(Peers, State) ->
    Peer = lists:nth(rand:uniform(length(Peers)), Peers),
    {ok, Pid} = newscast_peer:open(Peer),
    State#state{peer_ref = {Pid, undefined}}.

handle_gossip(RemoteTime, RemoteCache, State) ->
    #state{cache = LocalCache, agents = Agents} = State,
    News = newscast_cache:news(RemoteCache),
    [newscast_agent:news_update(Agent, News) || {Agent, _} <- Agents],
    newscast_cache:merge(RemoteTime, newscast_cache:entries(RemoteCache), State#state.clock, LocalCache).
