%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the specification of scrapers supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(webscraping_scrapers_sup).
-author("Krzysztof Spytkowski").
-behaviour(supervisor).
-define(INIT_SCRAPERS_FILENAME, "weather").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [?INIT_SCRAPERS_FILENAME]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications
%% @end
%%--------------------------------------------------------------------
init([Filename]) ->
	io:format("starting supervisor: ~s.~n", [?MODULE]),
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxSecondsBetweenRestarts = 20,
	Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = permanent,
	Shutdown = 2000,
	Type = worker,
	ScrapersSpecifications = read_scrapers_from_file(Filename),
	ChildrenSpecifications = create_children_specifications(ScrapersSpecifications, [], Restart, Shutdown, Type),
	{ok, { Flags, ChildrenSpecifications} }.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls a function that is responsible for reading scrapers 
%% descriptions from file
%% @end
%%--------------------------------------------------------------------
read_scrapers_from_file(Filename) ->
	parserCSV:parse_csv(Filename).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses given scrapers descriptions and creates scrapers 
%% specifications (supervisor's children)
%% @end
%%--------------------------------------------------------------------
create_children_specifications([], Scrapers, _Restart, _Shutdown, _Type) -> 
	Scrapers;
create_children_specifications([{Attribute, URLBegin, URLEnd, XPaths} | T], Scrapers, Restart, Shutdown, Type) ->
	{ok,[{atom,_,AttributeAtom}],_} = erl_scan:string(Attribute),
	NewScrapers = [{AttributeAtom, {webscraping_simple_scraper, start_link, [AttributeAtom, URLBegin, URLEnd, XPaths]}, Restart, Shutdown, Type, [webscraping_simple_scraper]} | Scrapers],
	io:format("reading scraper specification: -~s-.~n", [Attribute]),
	create_children_specifications(T, NewScrapers, Restart, Shutdown, Type).	
