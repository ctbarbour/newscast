-module(newscast_peer).

-export([open/1]).
-export([gossip/2]).
-export([close/1]).

-export([start_link/3]).
-export([proc_lib_init/4]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
          parent      :: pid(),
          owner       :: pid(),
          owner_ref   :: reference(),
          peer        :: {inet:ip_address(), inet:port_number()},
          socket      :: undefined | inet:socket(),
          opts        :: maps:map(),
          last_error  :: any(),
          current_ref :: undefined | reference()
         }).

open(Peer) ->
    supervisor:start_child(newscast_peer_sup, [self(), Peer, #{}]).

close(Pid) ->
    supervisor:terminate_child(newscast_peer_sup, Pid).

gossip(Pid, Payload) ->
    Ref = make_ref(),
    Pid ! {gossip, self(), Ref, Payload},
    Ref.

start_link(Owner, Peer, Opts) ->
    proc_lib:start_link(?MODULE, proc_lib_init, [self(), Owner, Peer, Opts]).

proc_lib_init(Parent, Owner, Peer, Opts) ->
    try
        ok = proc_lib:init_ack(Parent, {ok, self()}),
        OwnerRef = erlang:monitor(process, Owner),
        State = #state{parent = Parent, owner = Owner, owner_ref = OwnerRef, opts = Opts,
                       peer = Peer},
        connect(State, retries(Opts))
    catch
        _:normal ->
            exit(normal);
        _:shutdown ->
            exit(shutdown);
        _:{shutdown, _} = Reason ->
            exit(Reason);
        _:Reason ->
            exit({Reason, erlang:get_stacktrace()})
    end.

connect(#state{peer = {IpAddr, Port}} = State, Retries) ->
    TransportOpts = [binary, {active, false}, {packet, 4}, {nodelay, false}],
    case gen_tcp:connect(IpAddr, Port, TransportOpts, connect_timeout(State#state.opts)) of
        {ok, Socket} ->
            up(State, Socket);
        {error, Reason} ->
            retry(State#state{last_error = Reason}, Retries)
    end.

up(State, Socket) ->
    State#state.owner ! {newscast_peer, {up, self()}},
    loop(State#state{socket = Socket}).

retry(#state{last_error = Reason}, 0) ->
    exit({shutdown, Reason});
retry(State, Retries) ->
    retry_loop(State, Retries - 1).

retry_loop(#state{parent = Parent, opts = Opts} = State, Retries) ->
    _ = erlang:send_after(retry_timeout(Opts), self(), retry),
    receive
        retry ->
            connect(State, Retries);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {retry_loop, State, Retries})
    end.

down(State, Reason) ->
    State#state.owner ! {newscast_peer, {down, self()}},
    exit(Reason).

loop(#state{socket = Socket, owner = Owner, owner_ref = OwnerRef, parent = Parent} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {gossip, Owner, Ref, Payload} ->
            gen_tcp:send(Socket, encode(Payload)),
            loop(State#state{current_ref = Ref});
        {tcp, Socket, Data} ->
            close = handle_data(Data, State),
            ok = gen_tcp:close(Socket),
            down(State, normal);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            down(State, closed);
        {tcp_error, Socket, Reason} ->
            gen_tcp:close(Socket),
            down(State, {error, Reason});
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {loop, State});
        {'DOWN', OwnerRef, process, Owner, Reason} ->
            gen_tcp:close(Socket),
            owner_gone(Reason);
        Any ->
            error_logger:error_msg("Unmatched message: ~p~n", [Any]),
            loop(State)
    end.

handle_data(Data, #state{owner = Owner, current_ref = Ref}) ->
    RemoteCache = decode(Data),
    Owner ! {newscast_peer, {response, self(), Ref, RemoteCache}},
    close.

retries(Opts) ->
    maps:get(retry, Opts, 5).

connect_timeout(Opts) ->
    maps:get(connect_timeout, Opts, 5000).

retry_timeout(Opts) ->
    maps:get(retry_timeout, Opts, 5000).

decode(Data) ->
    binary_to_term(Data).

encode(Data) ->
    term_to_binary(Data).

owner_gone(normal) ->
    exit(normal);
owner_gone(shutdown) ->
    exit(shutdown);
owner_gone({shutdown, _} = Shutdown) ->
    exit(Shutdown);
owner_gone(Reason) ->
    exit({owner_gone, Reason}).

system_continue(_, _, {retry_loop, State, Retry}) ->
    retry_loop(State, Retry);
system_continue(_, _, {loop, State}) ->
    loop(State).

system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
