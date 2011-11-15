-module(master).
-export([start_system/1, wait/0, interface_distribute/0, pid_distribute/0, entask_crawlers/0, 
store_crawl_results/0]).

%% -----------------------------------------------------------------------------------------

%% starts the web crawler on one machine only

start_system(NumCrawlers) ->
    Pids = spawn_crawlers(NumCrawlers),
    PidDist = spawn_pid_distributer(Pids),
    Ints = spawn_interfaces(NumCrawlers),
    IntDist = spawn_interface_distributer(Ints),
    spawn_listener(PidDist, IntDist).


%% -----------------------------------------------------------------------------------------

%% spawns a process which distributes individual interface pids in a cycle

spawn_interface_distributer(Ints) -> 
    Dist = spawn(?MODULE, interface_distribute, []),
    Dist ! {self(), Ints},
    Dist.


%% -----------------------------------------------------------------------------------------

%% spawns a process which distributes individual crawler pids in a cycle

spawn_pid_distributer(Pids) -> 
    Dist = spawn(?MODULE, pid_distribute, []),
    Dist ! {self(), Pids},
    Dist.


%% -----------------------------------------------------------------------------------------

%% a process to distribute interface pids

interface_distribute() -> 
    receive
        {From, Ints} -> interface_distribute(Ints, 1);
        _ -> interface_distribute()
    end.


interface_distribute(Ints, Index) ->
    receive
        {From, int_request} -> 
            Int = lists:nth(Index, Ints),
            From ! {self(), {requested_int, Int}};
        {From, init_int_request} -> 
            Int = lists:nth(Index, Ints),
            From ! {self(), {requested_init_int, Int}};
        _ -> 
            interface_distribute(Ints, Index)
    end,
    ListLen = length(Ints),
    case (ListLen - Index) of
        0 -> interface_distribute(Ints, 1);
        _ -> interface_distribute(Ints, (Index + 1))
    end.


%% -----------------------------------------------------------------------------------------

%% a process to distribute crawler pids

pid_distribute() -> 
    receive
        {From, Pids} -> pid_distribute(Pids, 1);
        _ -> pid_distribute()
    end.


pid_distribute(Pids, Index) -> 
    receive
        {From, pid_request} -> 
            Pid = lists:nth(Index, Pids),
            From ! {self(), {requested_pid, Pid}};
        {From, init_pid_request} ->
            Pid = lists:nth(Index, Pids),
            From ! {self(), {requested_init_pid, Pid}};
        {From, length_pids} -> 
            Length = length(Pids),
            From ! {self(), {length_pids, Length}};
        _ -> 
            pid_distribute(Pids, Index)
    end,
    ListLen = length(Pids),
    case (ListLen - Index) of 
        0 -> pid_distribute(Pids, 1);
        _ -> pid_distribute(Pids, (Index + 1))
    end.


%% -----------------------------------------------------------------------------------------

%% spawns the master

spawn_listener(PidDist, IntDist) -> 
    Listener = spawn(?MODULE, wait, []),
    Listener ! {self(), {init, {PidDist, IntDist}}}.


%% -----------------------------------------------------------------------------------------

%% spawns individual web crawlers

spawn_crawlers(Num) -> 
    crawler:start(Num).


%% -----------------------------------------------------------------------------------------

%% spawns individual interfaces

spawn_interfaces(Num) -> 
    interface:start(Num).


%% -----------------------------------------------------------------------------------------

%% oversees the crawler system

wait() -> 
    receive
        {From, {init, {PidDist, IntDist}}} ->
            self() ! init,
            wait(PidDist, IntDist)
    end.

wait(PidDist, IntDist) -> 
    receive
        %% initialization of all crawlers
        init ->
            IntDist ! {self(), init_int_request};
        {From, {requested_init_int, Int}} -> 
            Int ! {self(), large_batch_unvisited_urls};
        {From, {large_batch_unvisited_urls, Urls}} -> 
            Ent = spawn(?MODULE, entask_crawlers, []),
            Ent ! {self(), {Urls, PidDist}};

        %% restock a crawler
        {From, new} ->
            IntDist ! {self(), int_request};
        {From, {requested_int, Int}} -> 
            Int ! {self(), std_batch_unvisited_urls};
        {From, {std_batch_unvisited_urls, Urls}} -> 
            submit_to_crawler(self(), PidDist, Urls);

        %% store feeds
        {From, {crawled, {NewLinks, Xml}}} ->
            Str = spawn(?MODULE, store_crawl_results, []),
            Str ! {self(), {IntDist, NewLinks, Xml}}
      end,
      wait(PidDist, IntDist).


%% -----------------------------------------------------------------------------------------

%% handle storing results of web crawler

store_crawl_results() ->
    receive
        {_, {IntDist, NewLinks, Xml}} -> 
            N = NewLinks,
            X = Xml,
            IntDist ! {self(), int_request}
    end,
    receive
        {_, {requested_int, Int}} ->
            Int ! {self(), {store_feeds, X}},
            Int ! {self(), {store_unvisited, N}}
    end.    


%% -----------------------------------------------------------------------------------------

%% dispatches the first round of crawlers

entask_crawlers() -> 
    receive
        {From, {Urls, PidDist}} -> 
            PidDist ! {self(), length_pids},
            Respondant = From
    end,
    receive
        {_, {length_pids, L}} -> 
            LengthPids = L
    end,
    LengthUrls = length(Urls),
    Div = (LengthUrls / LengthPids),
    ModSubLists = trunc(Div),
    lists:foldl(fun(X, {Count, List}) -> 
                    case Count of
                        LengthUrls ->
                            submit_to_crawler(Respondant, PidDist, [X|List]),
                            {1, []};
                        ModSubLists ->
                            submit_to_crawler(Respondant, PidDist, [X|List]),
                            {1, []};
                        _ -> 
                            {(Count + 1), [X|List]}
                    end
                end, {1, []}, Urls).


%% -----------------------------------------------------------------------------------------

%% submits a list of urls to a crawler, given a crawler distributer

submit_to_crawler(Respondant, PidDist, List) ->
    PidDist ! {self(), pid_request},
    receive
        {From, {requested_pid, Pid}} -> 
            Pid ! {Respondant, {url, List}}
    end.
                  
            
