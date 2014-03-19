-module(node).
-compile(export_all).

start() ->
    start(5).

start(N) ->
    random:seed(erlang:now()),
    start(N, self()).

start(0, BeginningNode) ->
    loop(BeginningNode);

start(N, BeginningNode) ->
    hello_I_am_a_node,
    FriendNode = spawn(node, start, [N-1, BeginningNode]),
    loop(FriendNode, [], []).

loop(Node) ->
    loop(Node, [],[]).

loop(Node, MyMessages, Workers) ->
    receive
        {request, write, N, Message} ->
            case is_it_my_message(MyMessages, {request, write, N, Message}) of
                yes ->
                    % I receive a write request and I wrote it ->
                    % - I complete the request myself
                    % - I say the request is complete
                    % (No need to check for a writer worker:
                    % the send function do it already.)
                    io:format("~w> I received a {request, write, ...}, I *DID* write it.~n", [self()]),
                    Worker = spawn(filewriter, start, []),
                    Worker ! {write, Message},
                    Node ! {ack, write, N, Message},
                    loop(Node, [{ack, write, N,Message}|MyMessages],
                         [Worker|Workers]);
                no ->
                    % I receive a write request and I didn't write it ->
                    % - I try to write it.
                    % - If I can't, I pass it along.
                    io:format("~w> I received a {request, write, ...}, I did *NOT* write it.~n", [self()]),

                    % Do I have a writer ?
                    case writer_worker(Workers) of
                        {yes, writer, Pid} ->
                            % I have a writer ->
                            % - I write it and I say I did it.
                            Pid ! {write, Message},
                            Node ! {ack, write, N, Message},
                            loop(Node, [{ack, write, N,Message}|MyMessages],
                                Workers);
                        _ -> %{no, writer} ->
                            % I don't have a writer ->
                            % - I pass along the request.
                            Node ! {request, write, N, Message},
                            loop(Node, MyMessages, Workers)
                    end
            end;
        {ack, write, N, Message} ->
            case is_it_my_message(MyMessages, {ack, write, N, Message}) of
                yes ->
                    % I receive a ack request and I wrote it ->
                    % - End of the road for this message.
                    io:format("~w> I received a {ack, write, ...}, I *DID* write it.~n", [self()]),
                    loop(Node, MyMessages, Workers);
                no ->
                    % I receive a ack request and I didn't write it ->
                    % - I pass along the request.
                    io:format("~w> I received a {ack, write, ...}, I did *NOT* write it.~n", [self()]),
                    Node ! {ack, write, N, Message},
                    loop(Node, MyMessages, Workers)
            end;
        {write, request, M} ->
            case writer_worker(Workers) of
                {yes, writer, Pid} ->
                    % I have a writer ->
                    % - I write it and I say I did it.
                    Pid ! {write, M},
                    Req = {ack, write, random(), M},
                    Node ! Req,
                    loop(Node, [Req|MyMessages], Workers);
                _ -> %{no, writer} ->
                    % I don't have a writer ->
                    % - I pass along the request.
                    Req = {request, write, random(), M},
                    Node ! Req,
                    loop(Node, [Req|MyMessages], Workers)
            end
    end.


writer_worker([]) ->
    {no, writer};

writer_worker([{Type, Pid}|_Tail]) when Type == writer ->
    {yes, writer, Pid};

writer_worker([_Head|Tail]) ->
    writer_worker(Tail).

is_it_my_message([], _Req) ->
    no;

is_it_my_message([{X, Y, Z, ZZ}|_T],
                 {RequestType, WorkerType, N, Message}) when X == RequestType,
                                                             Y == WorkerType,
                                                             Z == N,
                                                             ZZ == Message ->
    yes;

is_it_my_message(MyMessages, {RequestType, WorkerType, N, Message}) ->
    NewList = [{X, Y, Z, ZZ} || {X, Y, Z, ZZ} <- MyMessages, X == RequestType,
                                                             Y == WorkerType,
                                                             Z == N,
                                                             ZZ == Message],
    is_it_my_message(NewList, {RequestType, WorkerType, N, Message}).

random() ->
    Rnd = random:seed(erlang:now()),
    case Rnd of
        undefined ->
            random();
        _ ->
            {_, _, No} = Rnd,
            No
    end.

