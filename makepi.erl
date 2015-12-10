% approximate Pi to a certain precision using Leibniz formula
% usage: run makepi:getpi() in Erlang shell.
% by Luke Barker
-module(makepi).

%-compile(export_all). % for debug then replace with below later
-export([get_pi/0]).

% debugging macro for handiness; credit: http://blog.rusty.io/2011/01/13/beautiful-erlang-print/
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

% set the Needed Precision here 
% (a tenth of the required decimal places):
-define(PRECISION, 0.000001).
-define(STARTDENOM, 1).

% crude way to change the sign each time!
change_sign(X) ->
	X * -1.

% initialise the approximation to pi
get_pi() ->
	get_pi(?STARTDENOM, 1, 0).

% recursive method to ever closer approximate Pi
get_pi(Denom, Sign, ResultSoFar)  ->
	
	CurrentDenom = 1 / Denom,
	NewDenom = Denom + 2,
	
	% running total updated:
	Result = ResultSoFar + 4 * (Sign * CurrentDenom),

	% alternate plus and minus
	NewSign = change_sign(Sign),

	%debug only:
	%?PRINT(Result),
	%io:format("MyValue: ~p~n", [Result]),
	
	if
		% calculate the difference between results and previous result
		% divide by 2 as that should be the margin to pi.
		% (pi actual value sits between the two results)
		(abs(Result - ResultSoFar) / 2) > ?PRECISION -> 
		get_pi(NewDenom, NewSign, Result);

	% like an else condition, thus simply formatted 
	% output final approximation to chsoen accuracy:
	true -> 
		io:format("Approximated: ~.5f~n", [Result])
		
	end.






	











