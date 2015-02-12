%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the file's parser specification
%%% @end
%%%-------------------------------------------------------------------
-module(parserCSV).
-author("Krzysztof Spytkowski").

%% ===================================================================
%% API functions
%% ===================================================================
-export([parse_csv/1]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the process of reading a file 
%% @end
%%--------------------------------------------------------------------
parse_csv(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    process_file(Device, []).

%%--------------------------------------------------------------------
%% @doc
%% Reads and parses given file
%% @end
%%--------------------------------------------------------------------
process_file(Device, Accumulator) ->
	case io:get_line(Device, "") of
		eof -> 
			file:close(Device), 
			Accumulator;
		Line -> 
			NewAccumulator = process_one_line(Line, Accumulator),
			process_file(Device, NewAccumulator)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Process one line of a file
%% @end
%%--------------------------------------------------------------------
process_one_line(Line, Accumulator) -> 
	[Attribute, URLBegin, URLEnd, XPath1, XPath2, XPath3, XPath4, XPath5] = string:tokens(Line, ";\n"), 
	[{Attribute, URLBegin, URLEnd, [XPath1, XPath2, XPath3, XPath4, XPath5]} | Accumulator].
