-module(newscast_cache).

-export([new/1]).
-export([peers/1]).
-export([insert/4]).
-export([merge/4]).
-export([entries/1]).
-export([news/1]).
-export([from_list/2]).

-record(cache, {
          max_size     :: pos_integer(),
          entries = [] :: [entry()]
         }).

-type entry()   :: {newscast:peer(), integer(), newcast_agent:news()}.
-opaque cache() :: #cache{}.
-export_type([cache/0]).
-export_type([entry/0]).

-spec new(MaxSize) -> Cache when
      MaxSize :: pos_integer(),
      Cache   :: cache().

new(MaxSize) ->
    #cache{max_size = MaxSize}.

-spec from_list(MaxSize, Entries) -> Cache when
      MaxSize :: pos_integer(),
      Entries :: [entry()],
      Cache   :: cache().

from_list(MaxSize, Entries) ->
    TruncatedEntries = lists:sublist(lists:ukeysort(1, Entries), MaxSize),
    #cache{max_size = MaxSize, entries = TruncatedEntries}.

-spec peers(Cache) -> Peers when
      Cache :: cache(),
      Peers :: [newscast:peer()].

peers(#cache{entries = Entries}) ->
    [Peer || {Peer, _, _} <- Entries].

-spec insert(Peer, Timestamp, News, Cache) -> NewCache when
      Peer      :: newscast:peer(),
      Timestamp :: integer(),
      News      :: newscast_agent:news(),
      Cache     :: cache(),
      NewCache  :: Cache.

insert(Peer, Timestamp, News, #cache{entries = Entries} = Cache) ->
    Cache#cache{entries = lists:keystore(Peer, 1, Entries, {Peer, Timestamp, News})}.

-spec entries(Cache) -> Entries when
      Cache :: cache(),
      Entries :: [entry()].

entries(#cache{entries = Entries}) ->
    Entries.

-spec news(Cache) -> News when
      Cache :: cache(),
      News  :: newscast_agent:news().

news(#cache{entries = Entries}) ->
    lists:flatten([News || {_, _, News} <- Entries]).

-spec merge(RemoteTime, RemoteEntries, LocalTime, Cache) -> NewCache when
      RemoteTime    :: integer(),
      RemoteEntries :: [entry()],
      LocalTime     :: RemoteTime,
      Cache         :: cache(),
      NewCache      :: Cache.

merge(RemoteTime, RemoteEntries, LocalTime, #cache{entries = LocalEntries} = LocalCache) ->
    Sort = fun({_, A, _}, {_, B, _}) -> A > B end,
    Entries = localize(RemoteTime, LocalTime, RemoteEntries) ++ LocalEntries,
    MergedEntries = lists:ukeysort(1, lists:sort(Sort, Entries)),
    TruncatedEntries = lists:sublist(MergedEntries, LocalCache#cache.max_size),
    LocalCache#cache{entries = TruncatedEntries}.

-spec localize(RemoteTime, LocalTime, Entries) -> NewEntries when
      RemoteTime :: LocalTime,
      LocalTime  :: integer(),
      Entries    :: [entry()],
      NewEntries :: [entry()].

localize(RemoteTime, LocalTime, Entries) ->
    [{Peer, LocalTime - (RemoteTime - Timestamp), News} || {Peer, Timestamp, News} <- Entries].
