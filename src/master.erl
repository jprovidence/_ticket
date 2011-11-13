-module(master).

%% -----------------------------------------------------------------------------------------

%% starts the web crawler on one machine only

start_system(NumCrawlers) ->
    Pids = spawn_crawlers(NumCrawlers),
    PidDist = spawn_pid_distributer(Pids),
    Ints = spawn_interfaces(10),
    IntDist = spawn_interface_distributer(Ints),
    spawn_listener(PidDist, IntDist).


%% -----------------------------------------------------------------------------------------

%% spawns a process which distributes individual interface pids in a cycle

spawn_interface_distributer(Ints),
    Dist = spawn(?MODULE, interface_distribute, []),
    Dist ! {self(), Ints},
    Dist.


%% -----------------------------------------------------------------------------------------

%% spawns a process which distributes individual crawler pids in a cycle

spawn_pid_distributer(Pids),
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
        _ -> 
            interface_distribute(Ints, Index)
    end,
    ListLen = length(Ints)
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
    Listener = spawn(?MODULE, listen, []),
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

listen() -> 
    receive
        {From, init}} ->
            Urls = interface:batch_unvisited_urls({batch_size, large});
        {From, new} -> 
            Urls = interface:batch_unvisited_urls({batch_size, small});
        {From, {crawled, {NewLinks, XmlLinks}}} ->
            interface:
            
            
