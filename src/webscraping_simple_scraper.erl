%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the specification of a simple web scraper
%%% @end
%%%-------------------------------------------------------------------
-module(webscraping_simple_scraper).
-author("Krzysztof Spytkowski").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {url_begin="", url_end="", xpaths, attribute=""}).

%% ===================================================================
%% API Function Exports
%% ===================================================================

-export([start_link/4]).

%% ===================================================================
%% gen_server Function Exports
%% ===================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Attribute, URLBegin, URLEnd, XPaths) ->
    gen_server:start_link({local, Attribute}, ?MODULE, [Attribute, URLBegin, URLEnd, XPaths], []).

%% ===================================================================
%% gen_server Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Attribute, URLBegin, URLEnd, XPaths]) ->
	io:format("starting scraper: -~s-.~n", [Attribute]),
	process_flag(trap_exit, true),
    {ok, #state{url_begin=URLBegin, url_end=URLEnd, xpaths=XPaths, attribute=Attribute}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {noreply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({check_condition, Province, City, CollectorPid}, State) ->
	io:format("-~s- is scraping.~n", [State#state.attribute]),
	scrap(State#state.url_begin, State#state.url_end, State#state.xpaths, State#state.attribute, Province, City, CollectorPid),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
	io:format("terminating scraper: ~s.~n", [State#state.attribute]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Scrapes some information from a website and sends them to the 
%% Collector
%% @end
%%--------------------------------------------------------------------
scrap(URLBegin, URLEnd, XPaths, Attribute, Province, City, CollectorPid) ->
	URL = build_full_url(URLBegin, URLEnd, Province, City),
	Response = httpc:request(URL),
	case Response of
		{error, _Reason} -> 
			CollectorPid ! {bad_location, Province, City};
		{ok,{ _, _, Body}} ->
			Tree = mochiweb_html:parse(list_to_binary(Body)),
			Results = take_values(XPaths, Tree, []),
			FilteredResults = filter(Results, [], ""),
			CollectorPid ! {value, Attribute, FilteredResults}
	end.
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds full URL
%% @end
%%--------------------------------------------------------------------
build_full_url(URLBegin, URLEnd, Province, City) ->
	URLBegin ++ Province ++ "/" ++ City ++ URLEnd.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates list of values taken from a tree of website body 
%% (using xpaths)
%% @end
%%--------------------------------------------------------------------
take_values([], _Tree, Acc) ->
	Acc;
take_values([XPath | T], Tree, Acc) ->
	NewItem = mochiweb_xpath:execute(XPath, Tree),
	NewAcc = [unicode:characters_to_list(NewItem) | Acc],
	take_values(T, Tree, NewAcc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates list of filtered results
%% @end
%%--------------------------------------------------------------------
filter([], Acc, Unit) ->
	case Unit of
		"" -> [lists:reverse(Acc), "brak"];
		_ -> [lists:reverse(Acc), lists:concat(Unit)]
	end;
filter([H | T], Acc, _Unit) ->
	[H1 | Unit1] = string:tokens(H, "\n \t"),
	NewAcc = [H1 | Acc],
	filter(T, NewAcc, Unit1).
