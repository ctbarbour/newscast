-module(newscast_agent).

-behavior(gen_server).

-type news_item() :: any().
-type news()      :: [news_item()].
-opaque id()      :: atom() | string() | binary().
-export_type([id/0]).
-export_type([news/0]).
-export_type([news_item/0]).

-callback init(Args) -> Result when
      Result  :: {ok, State} | {stop, Reason} | ignore,
      State   :: any(),
      Reason  :: atom(),
      Args    :: any().

-callback id(State) -> ID when
      State :: any(),
      ID    :: id().
-optional_callbacks([id/1]).

-callback news_update(News, State0) -> Result when
      News   :: news(),
      Result :: {ok, State} | {stop, Reason},
      State  :: any(),
      State0 :: any(),
      Reason :: atom().

-callback get_news(State0) -> Result when
      Result   :: {ok, NewsItem, State} | {stop, Reason},
      NewsItem :: news_item(),
      Reason   :: any(),
      State    :: any(),
      State0   :: any().

-callback handle_DOWN(Reason, State0) -> Result when
      Reason :: any(),
      Result :: {ok, State},
      State0 :: any(),
      State  :: any().
-optional_callbacks([handle_DOWN/2]).

-callback handle_call(Request, From, State0) -> Result when
      Request :: any(),
      Result  :: {reply, Reply, State} | {noreply, State}
               | {stop, Reason, State} | {stop, Reason, Reply, State},
      Reply   :: any(),
      From    :: {pid(), reference()},
      State0  :: any(),
      Reason  :: any().
-optional_callbacks([handle_call/3]).

-callback handle_cast(Request, State) -> Result when
      Request :: any(),
      State   :: any(),
      Result  :: {noreply, NewState} | {stop, Reason, NewState},
      NewState :: any(),
      Reason   :: any().
-optional_callbacks([handle_cast/2]).

-callback handle_info(Info, State) -> Result when
      Info     :: any(),
      State    :: any(),
      Result   :: {noreply, NewState} | {stop, Reason, NewState},
      NewState :: any(),
      Reason   :: any().
-optional_callbacks([handle_info/2]).

-callback code_change(OldVsn, State, Extra) -> Result when
      OldVsn   :: Vsn | {down, Vsn},
      Vsn      :: any(),
      State    :: any(),
      Extra    :: any(),
      Result   :: {ok, NewState} | {error, Reason},
      NewState :: any(),
      Reason   :: any().
-optional_callbacks([code_change/3]).

-callback terminate(Reason, State) -> ok when
      Reason :: any(),
      State  :: any().
-optional_callbacks([terminate/2]).

-export([news_update/2]).
-export([get_news/1]).

-export([call/3]).
-export([cast/2]).
-export([start_link/2]).
-export([start_link/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          cbmod             :: module(),
          cbstate           :: any(),
          correspondent_ref :: reference()
         }).

call(Pid, Request, Timeout) ->
    gen_server:call(Pid, Request, Timeout).

cast(Pid, Request) ->
    gen_server:cast(Pid, Request).

start_link(Module, Args) ->
    gen_server:start_link(?MODULE, [Module, Args], []).

start_link(Name, Module, Args) ->
    gen_server:start_link(Name, ?MODULE, [Module, Args], []).

news_update(Pid, News) ->
    gen_server:cast(Pid, {'$newscast.news_update', News}).

get_news(Pid) ->
    gen_server:call(Pid, '$newscast.get_news').

init([Module, Args]) ->
    case Module:init(Args) of
        {ok, CbState} ->
            ID = Module:id(CbState),
            ok = newscast_correspondent:register(ID, self()),
            MRef = erlang:monitor(process, newscast_correspondent),
            {ok, #state{correspondent_ref = MRef, cbmod = Module, cbstate = CbState}};
        {stop, Reason} ->
            {stop, Reason};
        ignore ->
            ignore
    end.

handle_call('$newscast.get_news', _From, #state{cbmod = Mod, cbstate = CbState0} = State) ->
    case Mod:get_news(CbState0) of
        {ok, News, CbState} ->
            {reply, News, State#state{cbstate = CbState}};
        {stop, Reason} ->
            {stop, Reason}
    end;
handle_call(Request, From, State) ->
    case do_call(Request, From, handle_call, State) of
        {reply, Reply, CbState} ->
            {reply, Reply, State#state{cbstate = CbState}};
        {noreply, CbState} ->
            {noreply, State#state{cbstate = CbState}};
        {stop, Reason, CbState} ->
            {stop, Reason, State#state{cbstate = CbState}};
        no_call ->
            {noreply, State}
    end.

handle_cast({'$newscast.news_update', News}, #state{cbmod = Mod, cbstate = CbState0} = State) ->
    case Mod:news_update(News, CbState0) of
        {ok, CbState} ->
            {noreply, State#state{cbstate = CbState}};
        {stop, Reason} ->
            {stop, Reason}
    end;
handle_cast(Request, State) ->
    case do_call(Request, handle_cast, State) of
        {noreply, NewCbState} ->
            {noreply, State#state{cbstate = NewCbState}};
        {stop, Reason, NewCbState} ->
            {stop, Reason, State#state{cbstate = NewCbState}};
        no_call ->
            {noreply, State}
    end.

handle_info({'DOWN', MRef, process, _Pid, Reason}, State)
  when MRef =:= State#state.correspondent_ref ->
    case do_call(Reason, handle_DOWN, State) of
        {ok, CbState} ->
            {stop, {correspondent_down, Reason}, State#state{cbstate = CbState}};
        no_call ->
            {stop, {correspondent_down, Reason}, State}
    end;
handle_info(Info, State) ->
    case do_call(Info, handle_info, State) of
        {noreply, NewCbState} ->
            {noreply, State#state{cbstate = NewCbState}};
        {stop, Reason, NewCbState} ->
            {stop, Reason, State#state{cbstate = NewCbState}};
        no_call ->
            {noreply, State}
    end.

code_change(OldVsn, State, Extra) ->
    case do_call(OldVsn, Extra, code_change, State) of
        {ok, CbState} ->
            {ok, State#state{cbstate = CbState}};
        {error, Reason} ->
            {error, Reason};
        no_call ->
            {ok, State}
    end.

terminate(Reason, State) ->
    do_call(Reason, terminate, State),
    ok.

do_call(Request, Callback, #state{cbmod = Mod, cbstate = CbState}) ->
    case erlang:function_exported(Mod, Callback, 2) of
        true ->
            Mod:Callback(Request, CbState);
        false ->
            no_call
    end.

do_call(Request, From, Callback, #state{cbmod = Mod, cbstate = CbState}) ->
    case erlang:function_exported(Mod, Callback, 3) of
        true ->
            Mod:Callback(Request, From, CbState);
        false ->
            no_call
    end.
