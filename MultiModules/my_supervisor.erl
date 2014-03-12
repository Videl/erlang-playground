-module(my_supervisor).
-export([start/0]).

start() ->
    hello_I_am_a_supervisor,
    spawn(node, start, []).
