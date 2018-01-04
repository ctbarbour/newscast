-module(newscast_peer_service).

-export([start_link/3]).
-export([proc_lib_init/4]).
-export([accept/3]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
          socket,
          parent,
          acceptors,
          opts
         }).

start_link(IpAddr, Port, Opts) ->
    proc_lib:start_link(?MODULE, proc_lib_init, [self(), IpAddr, Port, Opts]).

proc_lib_init(Parent, IpAddr, Port, Opts) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    MinAcceptors = maps:get(min_accecptor, Opts, 2),
    TcpOpts = [binary, {packet, 4}, {ip, IpAddr},
               {reuseaddr, true}, {nodelay, true}, {active, false}],
    {ok, LSocket} = gen_tcp:listen(Port, TcpOpts),
    Acceptors = lists:foldl(
                  fun(_, Acc) ->
                          Pid = spawn_link(?MODULE, accept, [self(), LSocket, Opts]),
                          [{Pid} | Acc]
                  end, [], lists:seq(1, MinAcceptors)),
    loop(#state{socket = LSocket, parent = Parent, acceptors = Acceptors, opts = Opts}).

loop(#state{parent = Parent} = State) ->
    receive
        accepted ->
            Pid = spawn_link(?MODULE, accept, [self(), State#state.socket, State#state.opts]),
            loop(State#state{acceptors = [{Pid} | State#state.acceptors]});
        {'EXIT', _Pid, {error, emfile}} ->
            exit(emfile);
        {'EXIT', Pid, _Reason} ->
            case lists:keytake(Pid, 1, State#state.acceptors) of
                {value, _, Acceptors} ->
                    loop(State#state{acceptors = Acceptors});
                false ->
                    loop(State)
            end;
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {loop, State});
        _Other ->
            loop(State)
    end.

accept(Server, LSocket, Opts) ->
    case gen_tcp:accept(LSocket, maps:get(accept_timeout, Opts, 10000)) of
        {ok, Socket} ->
            Server ! accepted,
            {ok, Data} = gen_tcp:recv(Socket, 0, maps:get(receive_timeout, Opts, 60000)),
            PlainText = binary_to_term(Data),
            LocalCache = newscast_correspondent:gossip(PlainText),
            gen_tcp:send(Socket, term_to_binary(LocalCache)),
            gen_tcp:close(Socket);
        {error, timeout} ->
            accept(Server, LSocket, Opts);
        {error, econnaborted} ->
            accept(Server, LSocket, Opts);
        {error, closed} ->
            ok;
        {error, Reason} ->
            exit({error, Reason})
    end.

system_continue(_, _, {loop, State}) ->
    loop(State).

system_terminate(Reason, _, _, _) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
