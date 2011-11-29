-module(master).
-export([start_system/0, program_monitor/2, wait/2]).

%% -----------------------------------------------------------------------------------------

%% load the available machine list from disk. Load the current computation.
%% start ProgamMonitor, which oversees switching between 'States'. 

start_system() -> 
    Nodes = available_nodes(),
    CurComp = current_computation(),
    CurCount = current_count(),
    ProgMonitor = spawn(?MODULE, program_monitor, [CurComp, CurCount]),
    Mstr = spawn(?MODULE, wait, [ProgMonitor, CurComp]),
    Aggregator = spawn(aggregator, start_c_node, []),
    lists:map(fun(X) -> 
                  fully_load(X, CurComp, Mstr, Aggregator)
              end, Nodes),
    Mstr.


%% -----------------------------------------------------------------------------------------

%% collapses the initialisation/transition superposition based on the
%% provided directive @Computation@" 

fully_load(Node, Computation, Mstr, Aggregator) ->
    case Computation of
        "crawl" -> 
            systemwide_crawl(Node, true, {Mstr, Aggregator})
    end.


%% -----------------------------------------------------------------------------------------

%% recursively starts crawl processes on the provided node until the CPU is ~90%

systemwide_crawl(Node, Init, {Mstr, Aggregator}) when Init =:= true ->
    NameStr = lists:nth(1, string:tokens(Node, ",")),
    NodeName = list_to_atom(NameStr),
    spawn(NodeName, httpu, init_client, []),
    spawn(NodeName, cpu_sup, start, []),
    Pid = spawn(NodeName, crawler, crawl, [{Mstr, Aggregator}]),
    systemwide_crawl(Node, [Pid], {Mstr, Aggregator});

systemwide_crawl(Node, Pids, {Mstr, Aggregator}) when Pids =/= true ->
    NameStr = lists:nth(1, string:tokens(Node, ",")),
    NodeName = list_to_atom(NameStr),
    Pid = spawn(NodeName, crawler, crawl, [{Mstr, Aggregator}]),
    timer:sleep(300000),
    Pid ! {self(), status},
    receive
        {From, {status, Status}} -> 
            case assess_status(lists:nth(2, string:tokens(Node, ",")), Status) of
                ok -> systemwide_crawl(Node, [Pid|Pids], {Mstr, Aggregator});
                break -> Pids
            end
    end.


%% -----------------------------------------------------------------------------------------

%% determines whether or not the Node should start new processes based on CPU usage.

assess_status(StrLoad, Status) ->
    {MaxLoad, _} = string:to_integer(StrLoad),
    erlang:display(MaxLoad),
    erlang:display(Status),
    case (Status < MaxLoad) of
        true -> ok;
        false -> break
    end.


%% -----------------------------------------------------------------------------------------

%% convenience function to load the current state from disk 

current_computation() ->
    ComAry = fileu:read_by_line("../data/current_computation._ticket"),
    lists:nth(1, ComAry).


%% -----------------------------------------------------------------------------------------

%% convenience function to load the progress of the current state from disk

current_count() -> 
    CouAry = fileu:read_by_line("../data/current_count._ticket"),
    lists:nth(1, CouAry).


%% -----------------------------------------------------------------------------------------

%% convenience function to load currently available nodes from disk

available_nodes() -> 
    fileu:read_by_line("../data/nodes._ticket").


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
            case NewTicket > 1000000 of 
                true ->
                    From ! {self(), {progmon, stop, "crawl"}},
                    program_monitor("crawl", 0);
                false ->
                    From ! {self(), {progmon, ok}},
                    program_monitor(Computation, NewTicket)
            end
    end.


%% -----------------------------------------------------------------------------------------

%% Serves as a commuication channel between the user and system, monitor and individual 
%% processes, etc... 

wait(ProgMon, Mode) when Mode =/= shutdown ->
    receive
        shutdown ->
            perform_shutdown(ProgMon),
            wait(ProgMon, shutdown);

        {From, {crawler, Count}} ->
            case Mode of
                "crawl" ->
                    ProgMon ! {self(), {crawl, Count}},
                    receive
                        {_, {progmon, ok}} -> 
                            From ! {self(), ok};
                        {_, {progmon, stop, NewTask}} ->
                            From ! {self(), stop},
                            Mode = NewTask,
                            lists:map(fun(X) -> 
                                          fully_load(X, NewTask, self(), self()) %% THIS IS NOT CORRECT
                                      end, available_nodes())
                    end;
                _ ->
                    From ! {self(), stop}
            end
    end,
    wait(ProgMon, Mode);

wait(ProgMon, Shutdown) when Shutdown =:= shutdown->
    receive
        {From, _} -> From ! shutdown
    end,
    wait(ProgMon, shutdown).


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



    
