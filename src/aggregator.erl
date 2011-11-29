-module(aggregator).
-export([start_c_node/0]).

start_c_node() ->
    

aggregate() ->
    receive
        shutdown ->
            ok;

        {From, {xml, Xml}} ->
            From ! {self(), {xml, ok}}
    end.
