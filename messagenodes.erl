-module(messagenodes).
-export([start/1, helper_supervisor/1,helper_node/1 ]).

start(NbNodes) ->
	SuperPid = spawn_link(?MODULE, helper_supervisor, [NbNodes]),
    io:format("Supervisor Pid: ~w~n", [SuperPid]).

helper_supervisor(NbNodes) ->
    % create a new node
    NextPid = spawn_link(?MODULE, helper_node, [NbNodes]),
    % pass its Pid number to supervisor
    supervisor(NextPid).


supervisor(NextPid) ->
    io:format("Supervisor> Sending my message!~n"),
    NextPid ! {supervisor, self()},

    receive
	    {'EXIT', _Pid, Message} ->
	        io:format("(NUM_NODE,~w) Received extinction message: ~w~n",
                      [self(), Message]);
        M ->
            io:format("Message: ~w~n", [M])
    end.


helper_node(0) ->
		io:format("Last node. ~n"),
        receive
            {i_hope_it_will_never_come} ->
                exit(wow);
            Message ->
                io:format("Node (~w, ~w)> Received message: ~w~n", [0, self(),
                                                                   Message])
        after
            10000 ->
                exit(badend)
        end;


helper_node(NumNoeud) ->
    % create a new node
    NextPid = spawn_link(?MODULE, helper_node, [NumNoeud-1]),
    node(NumNoeud, NextPid).

node(NumNoeud, Pid) ->
	receive
		{supervisor, Message} ->
            io:format("Node (~w, ~w)> Received supervisor message! Sending it!~n",
                      [NumNoeud, self()]),
		    Pid ! {supervisor, Message};
        {'EXIT', _Pid, Message} ->
            io:format("Node (~w, ~w)> Received end of node! Message: ~w~n", [NumNoeud,
                                                                self(),Message])
	end,
	node(NumNoeud, Pid).
