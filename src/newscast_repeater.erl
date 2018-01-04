-module(newscast_repeater).
-behavior(newscast_agent).

-export([publish/2]).
-export([start_link/0]).
-export([init/1]).
-export([id/1]).
-export([get_news/1]).
-export([news_update/2]).
-export([handle_cast/2]).

publish(Pid, Item) ->
    newscast_agent:cast(Pid, {publish, Item}).

start_link() ->
    newscast_agent:start_link(?MODULE, []).

id(_) ->
    ?MODULE.

init(_) ->
    {ok, queue:new()}.

get_news(State0) ->
    case queue:out(State0) of
        {empty, State1} ->
            {ok, empty, State1};
        {{value, News}, State1} ->
            {ok, News, State1}
    end.

news_update(News, State0) ->
    State = lists:foldl(fun(N, Q) -> queue:in(N, Q) end, State0,
                        [Item || Item <- News,
                                 Item /= empty]),
    {ok, State}.

handle_cast({publish, Item}, State) ->
    {noreply, queue:in(Item, State)}.
