-module(interface).
-export([start/1, wait/0]).

%% -----------------------------------------------------------------------------------------

%% starts the specified number of interfaces

start(Number) ->
    Pid = spawn(?MODULE, wait, []),
    start(Number - 1, [Pid]).

start(N, L) when N =/= 0 ->
    Pid = spawn(?MODULE, wait, []),
    start(N - 1, [Pid|L]);

start(N, L) when N == 0 -> 
    L.


%% -----------------------------------------------------------------------------------------

%% waits to provide an interface to the Simple REST api

wait() -> 
    receive
        {From, large_batch_unvisited_urls} -> 
            {Headers, Body} = httpu:get_json_http("http://localhost:3000/detritus/pull?size=large"),
            {_, Links} = mochijson:decode(Body),
            From ! {self(), {large_batch_unvisited_urls, Links}};
        {From, std_batch_unvisited_urls} -> 
            {Headers, Body} = httpu:get_json_http("http://localhost:3000/detritus/pull?size=standard"),
            {_, Links} = mochijson:decode(Body),
            From ! {self(), {std_batch_unvisited_urls, Links}};
        {From, {store_feeds, Xml}} ->
            {Headers, Body} = httpu:get_http(Xml),
            {Response} = httpu:post_http("http://localhost:3000/arborage/receive", Body);
        {From, {store_unvisited, Urls}} -> 
            {Response} = httpu:post_http("http://localhost:3000/detritus/receive", Urls)
    end.
