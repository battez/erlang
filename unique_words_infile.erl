% Take a file input; output the unique items in the list. 
% Count how many unique items there are.
%
% usage:
% Pass a filename in same directory 
% run("Hamlet.txt")
% Takes a while but should output a big list and 
% the number of uniques
%
-module(unique_words_infile).
-author("John Luke Barker").
-date({2015, 12, 11}).

% needed functions:
-import(string, [tokens/2]).
-import(readfile, [readlines/1]).
-import(itemise, [uniq/1, uniq/2]).

-export([run/1]).


% the main calling function, reads the file and then
% goes thru it using process(), tallying an output list:
run(Myfile) -> 
	io:fwrite("Processing, please wait...~n", []),
	String = readlines(Myfile),
	process(String, []).

% stop pattern which will process with itemise:uniq and give 
% number of unique words in the given file:
process([], Result)->
	uniq(Result);

% recursive pattern that builds up output result 
% - a list with many elements
process([H|T], Result) ->

	% per line, whittle down to words without punctuation etc:
	% remove newlines first
	HstripNoNewlines = re:replace(H, "\\n+", "", [global,{return,list}]),
	
	% now remove misc unwanted characters
	Hstrip = re:replace(HstripNoNewlines, "[^a-z\s]", "", [global,{return,list}]),
	
	Tokens = tokens(Hstrip, " ") ,
	process(T, (Result ++ Tokens)).

	
  	
