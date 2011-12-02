-module(crawler).
-export([initialize_crawlers/1, crawl/2, crawl_master/2]).

initialize_crawlers(Capacity) ->
    spawn(cpu_sup, start, []),
    httpu:init_client(),
    CC = fileu:read_by_line("../data/current_count._ticket"),
    CurrentCount = list_to_integer(lists:nth(1, CC)),
    erlang:display("_ticket: Spawning crawl-master @ " ++ atom_to_list(node()) ++ "."),
    Master = spawn(?MODULE, crawl_master, [[], CurrentCount]),
    initialize_crawlers(Capacity, Master).

initialize_crawlers(Capacity, Master) ->
    Current = cpu_sup:avg5(),
    case Current > Capacity of
        true -> 
            ok;
        false ->
            erlang:display("_ticket: Spawning crawler-interface @ " ++ atom_to_list(node()) ++ "."),
            Interface = spawn(interface, interface, []),
            erlang:display("_ticket: Spawning crawler @ " ++ atom_to_list(node()) ++ "."),
            Pid = spawn(?MODULE, crawl, [Interface, Master]),
            Master ! {self(), {sumbit_crawl_pid, Pid}},
            timer:sleep(300000),
            initialize_crawlers(Capacity, Master)
    end.


crawl(Interface, Master) -> 
    self() ! no_message,
    receive
        shutdown ->
            Interface ! shutdown,
            ok;

        pause ->
            receive
                shutdown ->
                    Interface ! shutdown,
                    ok;
                resume ->
                    crawl(Interface, Master)
            end;

        continue ->
            self() ! no_message,
            crawl(Interface, Master);

        no_message ->
            Interface ! {self(), {link_request, 1000}},
            receive
                pause ->
                    receive
                        resume ->
                            receive
                                {_, {link_request, Links}} ->
                                    {NewLinks, Xml} = do_process(Links),
                                    Interface ! {self(), {crawled, NewLinks, Xml}},
                                    Master ! {self(), {crawl_count, 1000}}
                            end
                    end;
                {_, {link_request, Links}} ->
                    {NewLinks, Xml} = do_process(Links),
                    Interface ! {self(), {crawled, NewLinks, Xml}},
                    Master ! {self(), {crawl_count, 1000}}
            end,
            crawl(Interface, Master)
    end.


crawl_master(Crawlers, CurrentCount) ->
    receive
        shutdown ->
            fileu:write_by_line("../data/current_count._ticket", CurrentCount),
            lists:map(fun(X) ->
                          X ! shutdown
                      end, Crawlers),
            ok;
        {_, {submit_crawl_pid, Pid}} ->
            crawl_master([Pid|Crawlers], CurrentCount);
        {From, {crawl_count, Count}} ->
            crawl_data_process ! {self(), crawl_max},
            receive
                {_, {crawl_max, CrawlMax}} ->
                    case (Count + CurrentCount) < CrawlMax of
                        true ->
                            From ! {self(), continue},
                            crawl_master(Crawlers, (Count + CurrentCount));
                        false ->
                            lists:map(fun(X) ->
                                          X ! pause
                                      end, Crawlers),
                            clear_database(),
                            lists:map(fun(X) ->
                                          X ! resume
                                      end, Crawlers),
                            crawl_master(Crawlers, 0)
                    end
            end
    end.
    

clear_database() ->
    ok.


%% -----------------------------------------------------------------------------------------

%% Process the given urls and ship them to the Interface for Neo4j storage.

do_process(Urls) -> 
    {Html, Xml} = lists:foldl(fun(X, Acc) -> 
                                  segregate(X, Acc)
                              end, {[], []}, Urls),
    NewLinks = lists:map(fun(X) -> 
                             reduce_html(X)
                         end, Html),
    {NewLinks, Xml}.


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
        {_, X} ->
            erlang:display("success " ++ Url),
            Ret = X,
            receive
                {'DOWN', _, _, _, _} -> 
                    erlang:demonitor(Ref)
            end;
        {'DOWN', _, _, _, _} ->
            erlang:display("err " ++ Url),
            Ret = {err, err},
            erlang:demonitor(Ref)
    after 5000 ->
        exit(Pid, "unresponsive"),
        receive 
            {'DOWN', _, _, _, _} -> ok
        end,
        erlang:demonitor(Ref),
        Ret = {err, err}
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
    lists:map(fun(L) -> restring_and_fortify(L, Root) end, Reduced).


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
    binary_to_list(Ret).


%% -----------------------------------------------------------------------------------------

%% Converts bitstring to a std string to ensure it is not relative. Returns the std string.

restring_and_fortify(Url, Root) ->
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

