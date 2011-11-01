-module(crawler).

-define(UA, "Mozilla/5.0 (Erlang http:request)").

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
            New = do_process(List, From),
            From ! {self(), New};
        {From, status} -> 
            {ok, Pid} = cpu_sup:start(),
            From ! {self(), cpu_sup:avg5()}
    end,
    crawl().


%% -----------------------------------------------------------------------------------------

%% Visit each link, send back all feeds and new links

do_process(Urls, Mstr) -> 
    {Html, Xml} = lists:foldl(segregate, {[], []}, Urls),
    NewLinks = lists:map(reduce_html, Html),
    Mstr ! {self(), {NewLinks, Xml}}.


%% -----------------------------------------------------------------------------------------

%% place xml links and html content of non-xml links in a tuple

segregate(X, {Html, Xml}) -> 
    {Headers, Body} = get_http(X),
    [{_, Mime}] = lists:filter(is_content_type, Headers),
    {ok, Xmlre} = re:compile("text\/xml"),
    case re:run(Xmlre) of
        {match, _} -> Ret = [X|Xml];
        _ -> Ret = [Body|Html]
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

get_html(Url) -> 
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", ?UA}]}, [], []),
    {Headers, Body}.


%% -----------------------------------------------------------------------------------------

%% get all links from html content. 

reduce_html(Html) ->
    Soup = qrly_html:parse_string(Html),
    Reduced = lists:map(excise_links, qrly:filter(Soup, "a")),
    lists:flatten(Reduced).


%% find all the links in the given qrly filter response
excise_links({_, Content, _}) ->
    [{_, Link}] = lists:filter( fun({X, _}) -> X =:= <<"href">> end, Content),
    Link.


