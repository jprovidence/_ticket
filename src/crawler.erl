-module(crawler).
-export([crawl/1]).


%% -----------------------------------------------------------------------------------------

%% coordinates crawling of links. Crawls links in batches of 1000, checking with Master
%% to see if shutdown is received or crawl state terminated

crawl(Master) ->
    erlang:display("crawler booting up"),
    Interface = spawn(interface, interface, []),
    Interface ! {self(), {link_request, 1000}},
    crawl(Master, Interface).

crawl(Master, Interface) ->
    receive
        shutdown ->
            Interface ! shutdown,
            ok;
        {From, status} ->
            Cpu = cpu_sup:avg5(),
            From ! {self(), {status, Cpu}};
        {From, ok} -> 
            Interface ! {self(), {link_request, 1000}};
        {From, stop} -> 
            Interface ! shutdown,
            ok;
        {From, {link_request, Links}} -> 
            do_process(Links, Interface),
            Master ! {self(), {crawler, 1000}},
            crawl(Master, Interface)
    end.


%% -----------------------------------------------------------------------------------------

%% Process the given urls and ship them to the Interface for Neo4j storage.

do_process(Urls, Interface) -> 
    {Html, Xml} = lists:foldl(fun(X, Acc) -> 
                                  segregate(X, Acc)
                              end, {[], []}, Urls),
    NewLinks = lists:map(fun(X) -> 
                             reduce_html(X)
                         end, Html),
    Interface ! {self(), {crawled, {NewLinks, Xml}}}.


%% -----------------------------------------------------------------------------------------

%% place xml links and html content of non-xml links in a tuple

segregate(X, {Html, Xml}) -> 
    Result = safe_get(X),
    case Result of
        {err, err} -> 
            Ret = {Html, Xml};
        {Headers, Body} -> 
            [{_, Mime}] = lists:filter(fun(H) -> is_content_type(H) end, Headers),
            {ok, Xmlre} = re:compile("xml"),
            case re:run(Mime, Xmlre) of
                {match, _} -> Ret = {Html, [X|Xml]};
                _ -> Ret = {[{X, Body}|Html], Xml}
            end
    end,
    Ret.


%% -----------------------------------------------------------------------------------------

%% safely visit a url which may or may not be malformed

safe_get(Url) ->
    {Pid, Ref} = spawn_monitor(fun() -> 
                                   Resp = httpu:get_http(Url),
                                   receive
                                       {From, return} -> 
                                           From ! {self(), Resp}
                                   end
                               end),
    Pid ! {self(), return},
    receive
        {From, X} ->
            erlang:display("success" ++ Url),
            Ret = X,
            receive
                {'DOWN', _, _, _, _} -> 
                    erlang:demonitor(Ref)
            end;
        {'DOWN', _, _, _, _} ->
            erlang:display("err" ++ Url),
            Ret = {err, err},
            erlang:demonitor(Ref)
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

%% get all links from html content. Expand them if they are relative.

reduce_html({Url, Html}) ->
    Soup = qrly_html:parse_string(Html),
    Root = find_url_root(Url),
    Reduced = lists:map(fun(X) -> excise_links(X) end, qrly_html:filter(Soup, "a")),
    Flattened = lists:flatten(Reduced),
    lists:map(fun(L) -> restring_and_fortify(L, Root) end, Flattened).


%% -----------------------------------------------------------------------------------------

%% Get the root of the url given

find_url_root(Url) -> 
    {ok, Regex} = re:compile("(https?:\/\/.*?)\/"),
    case re:run(Url, Regex) of
        {match, [{_, _}, {_, Stop}]} -> {Root, _} = lists:split(Stop, Url);
        _ -> Root = []
    end,
    Root.
                                


%% -----------------------------------------------------------------------------------------

%% find all the links in the given qrly filter response

excise_links({_, Content, _}) ->
    case lists:filter(fun(X) -> matches_href(X) end, Content) of
        [{_, Link}] -> Ret = Link;
        [] -> Ret = []
    end,
    Ret.


%% -----------------------------------------------------------------------------------------

%% Converts bitstring to a std string to ensure it is not relative. Returns the std string.

restring_and_fortify(Link, Root) ->
    Url = bitstring_to_list(Link),
    {ok, Regex} = re:compile("https?:\/\/"),
    case re:run(Url, Regex) of 
        {match, _} -> Ret = Url;
        _ -> Ret = Root ++ Url
    end,
    Ret.

%% -----------------------------------------------------------------------------------------

%% returns true if this tuple is href, otherwise false

matches_href(X) -> 
    [H|_] = tuple_to_list(X),
    H =:= <<"href">>.
