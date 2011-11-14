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
            {Headers, Body} = httpu:get_http("http://localhost:3000/detritus?size=large"),
            BinaryBody = list_to_binary(Body),
            Links = mochijson2:decode(BinaryBody),
            From ! {self(), {large_batch_unvisited_urls, Links}};
        {From, std_batch_unvisited_urls} -> 
            {Headers, Body} = httpu:get_http("http://localhost:3000/detritus?size=standard"),
            BinaryBody = list_to_binary(Body),
            Links = mochijson2:decode(BinaryBody),
            From ! {self(), {std_batch_unvisited_urls, Links}};
        {From, {store_feeds, Xml}} -> 
            {Response} = httpu:post_http("http://localhost:3000/arborage", Xml);
        {From, {store_unvisited, Urls}} -> 
            {Response} = httpu:post_http("http://localhost:3000/rake", Urls)
    end.
