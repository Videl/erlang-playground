-module(fileopener).
-export([start/0]).
% man 3 file

start() ->
    hello_I_am_file_opener_node,
    {ok, F} = file:open("temp", [read]),
    loop(F).

loop(FileDescriptor) ->
    receive
        {readline, Pid} ->
            case file:read_line(FileDescriptor) of
                eof ->
                    Pid ! {self(), read, eof};
                {ok, Data} ->
                    Pid ! {self(), read, Data}
            end,
            loop(FileDescriptor);
        {descriptor, Pid} ->
            Pid ! {descriptor, "temp", FileDescriptor},
            loop(FileDescriptor);
        die ->
            file_opener_bye
    end.
