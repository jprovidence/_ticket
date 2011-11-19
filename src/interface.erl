-module(interface).
-export([interface/0]).

%% -----------------------------------------------------------------------------------------

%% waits to provide an interface to the Simple REST api

interface() ->
    receive
        shutdown -> 
            ok;
        {From, {link_request, Number}} -> 
            StrNum = integer_to_list(Number),
            {Headers, Body} = httpu:get_json_http("http://localhost:3000/detritus/pull?size=" ++ StrNum),
            {_, Links} = mochijson:decode(Body),
            From ! {self(), {link_request, Links}},
            interface();
        {From, {crawled, {NewLinks, Xml}}} -> 
            case length(Xml) of
                0 -> ok;
                _ -> httpu:post_http("http://localhost:3000/arborage/receive", Xml)
            end,
            Furls = lists:foldl(fun(X, Acc) -> 
                                    X ++ Acc
                                end, [], NewLinks),
            ToSend = lists:foldl(fun(X, Str) -> 
                                     case Str of 
                                         "" -> X;
                                         _ -> T = "," ++ Str, 
                                              X ++ T
                                     end
                                 end, "", Furls),
            httpu:post_http("http://localhost:3000/detritus/receive", ToSend),
            interface()
    end. 

