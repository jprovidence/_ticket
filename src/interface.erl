-module(interface).
-export([interface/0]).


interface() ->
    receive
        shutdown ->
            ok;

        {From, {link_request, Number}} ->
            erlang:display("_ticket: Processing link request"),
            sqlite3:open(temp_html, []),
            [_, {rows, Rows}] = sqlite3:sql_exec(temp_html,
                                "SELECT * FROM html WHERE processed=0 ORDER BY id ASC LIMIT ?;", [{1, Number}]),
            sqlite3:close(temp_html),
            Links = lists:map(fun({_, L, _}) ->
                                  binary_to_list(L)
                              end, Rows),
            From ! {self(), {link_request, Links}},
            interface();

        {_, {crawled, NewLinks, Xml}} ->
            erlang:display("_ticket: Processing link submission"),
            sqlite3:open(temp_html, []),
            lists:map(fun(List) ->
                          sqlite3:write_many(temp_html, 
                                             html, 
                                             [[{id, null}, {url, X}, {processed, 0}] || X <- List])
                      end, NewLinks),
            sqlite3:write_many(temp_html, xml, [[{id, null}, {url, X}, {processed, 0}] || X <- Xml]),
            sqlite3:close(temp_html),
            interface()
            
    end.
