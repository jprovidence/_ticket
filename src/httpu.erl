-module(httpu).
-export([init_client/0, get_http/1]).
-define(UA, "Mozilla/5.0 (Erlang http:request)").

%% -----------------------------------------------------------------------------------------

%% starts the inets http client

init_client() ->
    application:start(inets),
    ok.


%% -----------------------------------------------------------------------------------------

%% retrieves headers and body from a url

get_http(Url) -> 
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", ?UA}]}, [], []),
    {Headers, Body}.


%% -----------------------------------------------------------------------------------------

%% http post

post_http(Url, Data) -> 
    JsonData = mochijson2:encode(Data),
    http:request(post, {Url, [{"User-Agent", ?UA}], "text/json", JsonData}, [], []).



