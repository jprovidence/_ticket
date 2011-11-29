-module(interface).
-export([interface/1]).

%% -----------------------------------------------------------------------------------------

%% waits to provide an interface to the Simple REST api

interface(Aggregator) ->
    receive
        shutdown -> 
            ok;

        {From, {link_request, Number}} ->
            StrNum = integer_to_list(Number),
            erlang:display("link request" ++ StrNum),
            sqlite3:open(temp_html),
            [_, {rows, Rows}] = sqlite3:sql_exec(temp_html,
                "SELECT * FROM html WHERE processed=0 ORDER BY id ASC LIMIT " ++ StrNum ++ ";"),
            sqlite3:close(temp_html),
            Links = lists:map(fun({L, _}) -> 
                                  L
                              end, Rows),
            From ! {self(), {link_request, Links}},
            interface(Aggregator);

        {From, {crawled, {NewLinks, Xml}}} ->
            sqlite3:open(temp_html),
            lists:map(fun(X) -> 
                          sqlite3:write(temp_html, html, [{url, X}, {processed, 0}])
                      end, Html),
            sqlite3:close(temp_html),
            Aggregator ! {self(), {xml, Xml}},
            receive
                {_, {xml, ok}} -> ok
            end,
            From ! {self(), {crawled, ok}},
            interface(Aggregator)
    end. 

