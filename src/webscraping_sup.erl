%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the specification of top supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(webscraping_sup).
-author("Krzysztof Spytkowski").
-behaviour(supervisor).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
init([]) ->
	io:format("starting supervisor: ~s.~n", [?MODULE]),
	RestartStrategy = one_for_one,
	MaxRestarts = 100,
	MaxSecondsBetweenRestarts = 10,
	Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = permanent,
	Shutdown = 4000,
	Type = worker,
	ChildSrvSpecification = {webscraping_srv, {webscraping_srv, start_link, []}, Restart, Shutdown, Type, [webscraping_srv]},
	Type2 = supervisor,
	ChildSupSpecification = {webscraping_scrapers_sup, {webscraping_scrapers_sup, start_link, []}, Restart, Shutdown, Type2, [webscraping_scrapers_sup]},
	{ok, { Flags, [ChildSrvSpecification, ChildSupSpecification]} }.
