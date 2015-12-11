-module(readfile).
-author("John Luke Barker").
-date({2015, 12, 10}).

-export([readlines/1]).

% usage:
% readfile:readlines("hamlet.txt")

% credit for convert file to list
% https://erlangcentral.org/wiki/index.php/Read_File_to_List
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).
 
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        % force each line to lower case to avaoid Youth vs youth ambiguity etc
        % append and recurse
        Line -> get_all_lines(Device, Accum ++ [string:to_lower(Line)])
    end.


