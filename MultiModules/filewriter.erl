-module(filewriter).
-export([start/0]).
% man 3 file

start() ->
    hello_I_am_file_writer_node,
    {ok, F} = file:open("temp", [append, delayed_write, unicode]),
    % 64KB in buffer or
    % 2s timeout
    loop(F).

loop(FileDescriptor) ->
    receive
        {write, M} ->
            file:write(FileDescriptor, M ++ "\n"), % writes term M in standard output F
            loop(FileDescriptor);
        {descriptor, Pid} ->
            % get file descriptor
            Pid ! {descriptor, "temp", FileDescriptor},
            loop(FileDescriptor);
        die ->
            ok = file:close(FileDescriptor),
            file_writer_bye
    end.
