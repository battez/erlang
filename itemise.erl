% Given a list, show only the unique items in the list. 
% Count how many unique items there are.
%
% usage:
% pass a non-empty list as argument to establish its unique items, e.g.:
% uniq([1,2,2,3])
% = [1,2,3] ~ 3 items.
%
-module(itemise).
-author("John Luke Barker").
-date({2015, 12, 09}).

-export([uniq/1, uniq/2]).


% debugging macro for handiness; credit: http://blog.rusty.io/2011/01/13/beautiful-erlang-print/
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.



% initialise pattern:
uniq([H|T]) ->

	% Output is our eventual list of uniques
	Output = [H],
	uniq(T, Output).

% the main recusrive pattern, identifies any new uniques and appends them:
uniq([H|T], Output) ->
	
	% check the difference between two lists. 
	Diff = Output -- [H],

	% if lefthand (ie Diff's assigned value) remains the same, 
	% then H is not already in it --
	% so we can add to a new variable and recurse on that.
	if Diff == Output ->
		NewOutput = Output ++ [H];
	true -> 
		NewOutput = Output
	end,

	% keep on trucking...
	uniq(T, NewOutput);

% the stop pattern, prints out a count then stops:
uniq([], Output) ->
	io:format("Uniques List: ~p~n", [Output]),
	each(Output, 0),
	ok.

% simple counter code to output total number items:
each([_|T], Total) ->
	NewTotal = Total + 1,
	each(T, NewTotal);

% display final total:
each([], Final)->
	io:format("Total: ~p", [Final]),
	io:fwrite(" unique item(s), "),
	[].


	