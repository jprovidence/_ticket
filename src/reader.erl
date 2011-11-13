-module(reader).
-include_lib("xmerl/include/xmerl.hrl").
-export([start/1]).


%% -----------------------------------------------------------------------------------------

%% Start @Number@ reader processes.
 
start(Number) ->
    httpu:init_client(),
    Pid = spawn(?MODULE, read, []),
    start(Number - 1, [Pid]).

start(N, L) when N =/= 0 ->
    Pid = spawn(?MODULE, read, []),
    start(N - 1, [Pid|L]);

start(N, L) when N == 0 -> 
    L.


%% -----------------------------------------------------------------------------------------

%% start reader processes on multiple nodes
%% @Nodes@ => {Node Name, Number} where @Number@ is the count of processes to start

start_dist(Nodes) ->
    lists:map(fun({Name, Num}) -> ct_rpc:call(Name, reader, start, [Num], 5000) end, 
              Nodes).


%% -----------------------------------------------------------------------------------------

%% mailbox protocol for reader
%% {_, {read _}} -> process feeds for the first time. 
%% {_, status}   -> get cpu usage stats for this node.

read() -> 
    receive
        {From, {read, List}} ->
            do_process(List, From),
            From ! {self(), new};
        {From, status} -> 
            {ok, _} = cpu_sup:start(),
            From ! {self(), cpu_spu:avg5()}
    end,
    parse().


%% -----------------------------------------------------------------------------------------

%% delegate processing the Url into a list of lists of items or entries, the post to the/a 
%% database actor
 
do_process(List, Mstr) ->
    lists:foldl(fun(X, Acc) -> visit_parse(X, Acc) end, {[], []}, List).


%% -----------------------------------------------------------------------------------------

%% Process a single Url into a list of items or entries. To be used in with a map function

visit_parse(Url, {Rss, Atom}) ->
    {_, Body} = httpu:get_http(Url),
    {Type, Items} = parse(Body),
    case Type of 
        atom_xml -> Ret = {Rss, [{Url, Items}|Atom]}};
        rss_xml  -> Ret = {[{Url, Items}|Rss], Atom}
    end,
    Ret.


%% -----------------------------------------------------------------------------------------

%% parse xml content into xmerl and infer the type

parse(Xml) ->
    {Content, _} = xmerl_scan:string(Xml),
    List = lists:reverse(extract(Content, [])),


extract(Cont, List) when is_record(Cont, xmlElement) ->
    case Cont#xmlElement.name of 
        title -> 
    


