-module(fileu).
-export([read_by_line/1, write_by_line/2]).

%% -----------------------------------------------------------------------------------------

%% reads the given file, returns list of all lines

read_by_line(Filename) -> 
    {ok, File} = file:open(Filename, [raw, read, read_ahead]),
    Lines = perform_read([], File),
    file:close(File),
    Lines.


%% -----------------------------------------------------------------------------------------

%% does the work of reading a file

perform_read(Lines, File) -> 
    case file:read_line(File) of 
        {ok, Line} -> 
            perform_read([Line|Lines], File);
        eof ->
            Slines = lists:map(fun(X) -> string:strip(X, right, $\n) end, Lines),
            lists:reverse(Slines)
    end.


%% -----------------------------------------------------------------------------------------

%% writes to a file

write_by_line(Filename, Str) -> 
    {ok, File} = file:open(Filename, [write]),
    io:format(File, Str),
    file:close(File).
    
