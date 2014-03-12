-module(hotload).
-export([server/2, upgrade/1]).
 
server(State, N) ->
    receive
	update ->
	    NewState = ?MODULE:upgrade(State),
	    ?MODULE:server(NewState);  %% loop in the new version of the module
	action ->
	    io:format("Plus~n", []),
	    server(State, N+1);
	SomeMessage ->
	    io:format("Weird message, doing nothing.~n", []),
	    server(State)  %% stay in the same version no matter what.
    end.
 
upgrade(OldState) ->
%% transform and return the state here.