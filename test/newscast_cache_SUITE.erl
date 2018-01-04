-module(newscast_cache_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([merge/1]).

all() ->
    [merge].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(merge, Config) ->
    ATime = 5,
    AEvents = [{{{127,0,0,1}, 5000}, 0, <<"1">>},
               {{{127,0,0,1}, 5001}, 1, <<"2">>},
               {{{127,0,0,1}, 5002}, 2, <<"3">>},
               {{{127,0,0,1}, 5003}, 4, <<"4">>},
               {{{127,0,0,1}, 5004}, 5, <<"5">>}],
    A = newscast_cache:from_list(3, AEvents),
    BTime = 300,
    BEvents = [{{{127,0,0,1}, 5003}, 298, <<"6">>},
               {{{127,0,0,1}, 5004}, 297, <<"7">>},
               {{{127,0,0,1}, 5005}, 296, <<"8">>},
               {{{127,0,0,1}, 5006}, 295, <<"9">>},
               {{{127,0,0,1}, 5007}, 294, <<"10">>}],
    B = newscast_cache:from_list(3, BEvents),
    [{a, {ATime, A}}, {b, {BTime, B}} | Config].

end_per_testcase(_, Config) ->
    Config.

merge(Config) ->
    {ATime, A} = ?config(a, Config),
    {BTime, B} = ?config(b, Config),
    BMerged = newscast_cache:merge(ATime, newscast_cache:entries(A), BTime, B),
    AMerged = newscast_cache:merge(BTime, newscast_cache:entries(B), ATime, A),
    true = newscast_cache:news(AMerged) =:= newscast_cache:news(BMerged).
