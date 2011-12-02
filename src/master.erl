-module(master).
-export([start_up/0, wait_serve_crawl_data/5, create_db/0, seed_db/0]).

start_up() ->
    {CrawlerNodes, CrawlMax, HaskellNode} = read_config(),
    register_haskell_node(HaskellNode),
    CrawlerPids = lists:map(fun(X) ->
                                Node = list_to_atom(lists:nth(1, string:tokens(X, ","))),
                                Capacity = list_to_integer(lists:nth(2, string:tokens(X, ","))),
                                spawn(Node, crawler, initialize_crawlers, [Capacity])
                            end, CrawlerNodes),
    register_crawl_data_process(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode),
    register_master_process(),
    erlang:display("_ticket: Master Initialization Complete @ " ++ atom_to_list(node()) ++ "."),
    assume_master_role().


register_haskell_node(NodeName) ->
    Pid = spawn(NodeName, ?MODULE, proxy_haskell, []),
    register(haskell_node, Pid).


proxy_haskell() ->
    receive
        shutdown ->
            ok;
        {From, {xml, Xml}} ->
            From ! {self(), {haskell, ok}}
    end.


register_master_process() ->
    register(master_process, self()).


register_crawl_data_process(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode) ->
    Pid = spawn(?MODULE, wait_serve_crawl_data, [CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode]),
    register(crawl_data_process, Pid).


wait_serve_crawl_data(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode) ->
    receive
        shutdown -> 
            ok;
        {From, crawler_nodes} ->
            From ! {self(), {crawler_nodes, CrawlerNodes}},
            wait_serve_crawl_data(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode);
        {From, crawler_pids} ->
            From ! {self(), {crawler_pids, CrawlerPids}},
            wait_serve_crawl_data(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode);
        {From, crawl_max} ->
            From ! {self(), {crawl_max, CrawlMax}},
            wait_serve_crawl_data(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode);
        {From, haskell_node} ->
            From ! {self(), {haskell_node, HaskellNode}},
            wait_serve_crawl_data(CrawlerNodes, CrawlerPids, CrawlMax, HaskellNode)
      end.


read_config() ->
    CrawlerNodes = fileu:read_by_line("../data/crawl_nodes._ticket"),
    CrawlMax = list_to_integer(lists:nth(1, fileu:read_by_line("../data/crawl_max._ticket"))),
    HaskellNode = list_to_atom(lists:nth(1, fileu:read_by_line("../data/haskell_node._ticket"))),
    {CrawlerNodes, CrawlMax, HaskellNode}.


create_db() ->
    sqlite3:open(temp_html, ["/home/providence/Dropbox/_ticket/_ticket/data/temp_html.db"]),
    TableData = [{id, integer, [{primary_key, [asc, autoincrement]}]}, 
                 {url, text, [not_null]}, 
                 {processed, integer, [not_null]}],
    ok = sqlite3:create_table(temp_html, html, TableData),
    ok = sqlite3:create_table(temp_html, xml, TableData),
    sqlite3:close(temp_html),
    erlang:display("_ticket: Database initialized.").


seed_db() ->
    Seeds = fileu:read_by_line("../data/database_seed._ticket"),
    sqlite3:open(temp_html, ["/home/providence/Dropbox/_ticket/_ticket/data/temp_html.db"]),
    lists:map(fun(X) ->
                  sqlite3:write(temp_html, html, [{url, X}, {processed, 0}])
              end, Seeds),
    sqlite3:close(temp_html),
    erlang:display("_ticket: Database seeded.").


assume_master_role() ->
    receive
        shutdown ->
            crawl_data_process ! {self(), crawler_pids},
            crawl_data_process ! shutdown,
            receive
                {_, {crawler_pids, CrawlerPids}} ->
                    lists:map(fun(X) ->
                                  X ! shutdown
                              end, CrawlerPids)
            end
    end.


