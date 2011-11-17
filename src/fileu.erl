-module(fileu).

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
            lists:map(fun(X) -> string:strip(X, right, $\n) end, Lines),
            lists:reverse(Lines)
    end.
