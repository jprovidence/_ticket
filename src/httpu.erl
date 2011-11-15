-module(httpu).
-export([init_client/0, get_http/1, get_json_http/1, post_http/2]).
-define(UA, "Mozilla/5.0 (Erlang http:request)").

%% -----------------------------------------------------------------------------------------

%% starts the inets http client

init_client() ->
    application:start(inets),
    ok.


%% -----------------------------------------------------------------------------------------

%% retrieves headers and body from a url

get_http(Url) -> 
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", ?UA}]},
        [{relaxed, true}, {timeout, 60000}], []),
    {Headers, Body}.


%% -----------------------------------------------------------------------------------------

%% specifically retrieves json via a get request

get_json_http(Url) ->
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", ?UA}, 
        {"Accept", "application/json"}]}, [], []),
    {Headers, Body}.


%% -----------------------------------------------------------------------------------------

%% http post

post_http(Url, Data) ->
    %%JsonData = mochijson2:encode({struct, [{urls, [list_to_binary(M) || M <- Data]}]}),
    Encoded = escape_uri(Data),
    http:request(post, {Url, [{"User-Agent", ?UA}], "text/plain", Encoded}, [], []),
    {ok}.


%% -----------------------------------------------------------------------------------------

%% uri encode data

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].


