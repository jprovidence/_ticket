-module(master).

%% -----------------------------------------------------------------------------------------

%% load the available machine list from disk. Load the current computation.
%% start ProgamMonitor, which oversees switching between 'States'. 

start_system() -> 
    Nodes = available_nodes(),
    CurComp = current_computation(),
    CurCount = current_count(),
    ProgMonitor = spawn(?MODULE, program_monitor, [CurCount]),
    Mstr = spawn(?MODULE, wait, [ProgMonitor]),
    lists:map(fun(X) -> 
                  fully_load(X, CurComp, Mstr)
              end, Nodes),
    Mstr.


%% -----------------------------------------------------------------------------------------

%% collapses the initialisation/transition superposition based on the
%% provided directive @Computation@" 

fully_load(Node, Computation, Mstr) ->
    case Computation of
        "crawl" -> 
            systemwide_crawl(Node, true, Mstr)
    end.


%% -----------------------------------------------------------------------------------------

%% recursively starts crawl processes on the provided node until the CPU is ~90%

systemwide_crawl(Node, Init, Mstr) when Init =:= true ->
    NodeName = lists:nth(1, string:tokens(Node, ",")),
    spawn(NodeName, httpu, init_client, []),
    Pid = spawn(NodeName, crawler, crawl, [Mstr]),
    systemwide_crawl(Node, [Pid]);

systemwide_crawl(Node, Pids, Mstr) when Pids =/= true ->
    NodeName = lists:nth(1, string:tokens(Node, ",")),
    Pid = spawn(NodeName, crawler, crawl, [Mstr]),
    Pid ! {self(), status},
    receive
        {From, {status, Status}} -> 
            case assess_status(lists:nth(2, string:tokens(Node, ",")), Status) of
                ok -> systemwide_crawl(Node, [Pid|Pids]);
                break -> Pids
            end
    end.


%% -----------------------------------------------------------------------------------------

%% determines whether or not the Node should start new processes based on CPU usage.

assess_status(MaxLoad, Status) ->
    case (Status < MaxLoad) of
        true -> ok;
        false -> break
    end.


%% -----------------------------------------------------------------------------------------

%% convenience function to load the current state from disk 

current_computation() ->
    ComAry = fileu:read_by_line("./data/current_computation._ticket"),
    lists:nth(1, ComAry).


%% -----------------------------------------------------------------------------------------

%% convenience function to load the progress of the current state from disk

current_cout() -> 
    CouAry = fileu:read_by_line("./data/current_count._ticket"),
    lists:nth(1, CouAry).


%% -----------------------------------------------------------------------------------------

%% convenience function to load currently available nodes from disk

available_nodes() -> 
    fileu:read_by_line("./data/nodes._ticket").


%% -----------------------------------------------------------------------------------------

%% this function loops in its own process. It coordinates transitions between
%% computational states

program_monitor(Computation, Ticket) ->
    receive
        shutdown -> 
            ok;
        {From, ticket} -> 
            From ! {From, {ticket, Computation, Ticket}};
        {From, {crawler, Count}} ->
            NewTicket = Ticket + Count,
            case NewTicket < 1000000000 of 
                true ->
                    From ! {self(), {progmon, stop, "stitch graph"}]},
                    program_monitor("stitch graph", 0);
                false ->
                    From ! {self(), {progmon, ok}},
                    program_monitor(Computation, NewTicket)
            end
    end.


%% -----------------------------------------------------------------------------------------

%% Serves as a commuication channel between the user and system, monitor and individual 
%% processes, etc... 

wait(ProgMon) ->
    receive
        shutdown ->
            perform_shutdown(ProgMon),
            wait(ProgMon, shutdown);

        {From, {crawler, Count}} ->
            ProgMon ! {self(), {crawl, Count}},
            receive
                {_, {progmon, ok}} -> 
                    From ! {self(), ok};
                {_, {progmon, stop, NewTask}} ->
                    From ! {self(), stop},
                    lists:map(fun(X) ->
                                  fully_load(X, NewTask, self())
                              end, available_nodes())
            end
    end,
    wait(ProgMon). 

wait(ProgMon, Shutdown) ->
    receive
        {From, _} -> From ! shutdown
    end,
    wait(ProgMon, Shutdown).


%% -----------------------------------------------------------------------------------------

%% shuts down all ProgMon, saves current state and progress 

perform_shutdown(ProgMon) ->
    Ticket = ProgMon ! {self(), ticket},
    receive
        {_, {ticket, Computation, Ticket}} -> 
            fileu:write_by_line("./data/current_computation._ticket", Computation),
            fileu:write_by_line("./data/current_count._ticket", Ticket)
    end,
    ProgMon ! shutdown.



    
