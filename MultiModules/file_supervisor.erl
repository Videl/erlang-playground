-module(file_supervisor).
-export([start/0]).

start() ->
    hello_I_am_supervisor,
    helper_supervisor().

helper_supervisor() ->
    Writer = spawn(filewriter, start, []),
    Reader = spawn(fileopener, start, []),
    io:format("I am ~p, and main writer is ~p. Mean reader is ~p.~n", [self(),
                                                                       Writer,
                                                                      Reader]),
    supervisor([{node, writer, Writer}, {node, reader, Reader}], 0).

supervisor(Nodes, N) ->
    receive
        read ->
            io:format("Reading: ~n"),
            Reader = get_reader(Nodes),
            Receiver = spawn(await_response()),
            Reader ! {readline, Receiver},
            supervisor(Nodes, N);
        die ->
            send_to_all_nodes(Nodes, die),
            exit("Received exit message.")
        after
            5000 ->
                io:format("Send write request.~n"),
                Writer = get_writer(Nodes),
                Writer ! {write, [transformation(N)]},
                supervisor(Nodes, N+1)
    end.


get_writer(Nodes) ->
    Pids = [ X || {node, writer, X} <- Nodes ],
    [Pid|_Rest] = Pids,
    Pid.

get_reader(Nodes) ->
    Pids = [ X || {node, reader, X} <- Nodes ],
    [Pid|_Rest] = Pids,
    Pid.

await_response() ->
   receive
       {_Pid, read, Data} ->
           io:format("~p~n", [Data])
    end.

send_to_all_nodes([], _Message) ->
    ok;

send_to_all_nodes([Node|Rest], Message) ->
    {_Name, _Job, Pid} = Node,
    Pid ! Message,
    send_to_all_nodes(Rest, Message).

transformation(A) ->
    A rem 48 + (122 - 48 + 1).
