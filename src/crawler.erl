-module(crawler).
-export([start/1, start/2, start_dist/1, crawl/0]).
-define(UA, "Mozilla/5.0 (Erlang http:request)").

%% This represents a process that will expand the 'data-horizon'.
%% Typically, many of these will be started on different nodes.
%% Its purpose is to take a list of unvisited urls and classify them 
%% as either XML or HTML. XML links are returned as is, whereas HTML 
%% links are visited and all links they contain harvested for the next
%% cycle

%% -----------------------------------------------------------------------------------------

%% Start the number of crawlers requested, return list of pids

start(Number) ->
    init_client(), 
    Pid = spawn(?MODULE, crawl, []),
    start(Number - 1, [Pid]).

start(N, L) when N =/= 0 ->
    Pid = spawn(?MODULE, crawl, []),
    start(N - 1, [Pid|L]);

start(N, L) when N == 0 -> 
    L.


%% -----------------------------------------------------------------------------------------

%% Start the number of crawlers requested on the nodes given.
%% @Nodes@ => [{node@name, number_to_start}, ...].
%% Returns list of Pids

start_dist(Nodes) ->
    lists:map(fun({Name, Num}) -> ct_rpc:call(Name, crawler, start, [Num], 5000) end, 
              Nodes).


%% -----------------------------------------------------------------------------------------

%% starts the inets http client

init_client() ->
    application:start(inets),
    ok.


%% -----------------------------------------------------------------------------------------

%% Either pull and parse html from a given link list or respond
%% With this node's work level

crawl() -> 
    receive
        {From, {url, List}} -> 
            do_process(List, From),
            From ! {self(), new};
        {From, status} -> 
            {ok, _} = cpu_sup:start(),
            From ! {self(), cpu_sup:avg5()}
    end,
    crawl().


%% -----------------------------------------------------------------------------------------

%% Visit each link, send back all feeds and new links

do_process(Urls, Mstr) -> 
    {Html, Xml} = lists:foldl(fun(X, Y) -> segregate(X, Y) end, {[], []}, Urls),
    NewLinks = lists:map(fun(X) -> reduce_html(X) end, Html),
    Mstr ! {self(), {NewLinks, Xml}}.


%% -----------------------------------------------------------------------------------------

%% place xml links and html content of non-xml links in a tuple

segregate(X, {Html, Xml}) -> 
    {Headers, Body} = get_http(X),
    [{_, Mime}] = lists:filter(fun(H) -> is_content_type(H) end, Headers),
    {ok, Xmlre} = re:compile("text\/xml"),
    case re:run(Mime, Xmlre) of
        {match, _} -> Ret = {Html, [X|Xml]};
        _ -> Ret = {[Body|Html], Xml}
    end,
    Ret.
    

%% -----------------------------------------------------------------------------------------

%% determine if this tuple represents the content-type param

is_content_type({K, _}) -> 
    case K of
        "content-type" -> true;
        _ -> false
    end.


%% -----------------------------------------------------------------------------------------

%% retrieves headers and body from a url

get_http(Url) -> 
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", ?UA}]}, [], []),
    {Headers, Body}.


%% -----------------------------------------------------------------------------------------

%% get all links from html content. 

reduce_html(Html) ->
    Soup = qrly_html:parse_string(Html),
    Reduced = lists:map(fun(X) -> excise_links(X) end, qrly_html:filter(Soup, "a")),
    lists:flatten(Reduced).


%% -----------------------------------------------------------------------------------------

%% find all the links in the given qrly filter response

excise_links({_, Content, _}) ->
    case lists:filter(fun(X) -> matches_href(X) end, Content) of
        [{_, Link}] -> Ret = Link;
        [] -> Ret = []
    end,
    Ret.

matches_href(X) -> 
    [H|_] = tuple_to_list(X),
    H =:= <<"href">>.
