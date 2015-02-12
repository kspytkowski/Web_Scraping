%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the file's parser specification
%%% @end
%%%-------------------------------------------------------------------
-module(webscraping_app).
-author("Krzysztof Spytkowski").
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/0. It starts all needed dependencies and next 
%% calls application:start/1.
%% @end
%%--------------------------------------------------------------------
start() ->
	start_deps(webscraping, permanent).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and starts the top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	io:format("starting application: ~s.~n", [?MODULE]),
	webscraping_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped.
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	io:format("stopping application: ~s.~n", [?MODULE]),
	exit(whereis(webscraping_sup), shutdown),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts all dependencies needed by webscraping application
%% @end
%%--------------------------------------------------------------------
start_deps(App, Type) ->
	case application:start(App, Type) of
		ok ->
			io:format("dependency started: ~s.~n", [App]),
			ok;
		{error, {not_started, Dep}} ->
			io:format("starting dependency: ~s.~n", [Dep]),
			start_deps(Dep, Type),
			start_deps(App, Type);
		{error, {already_started, _App}} ->
			io:format("dependency already started: ~s.~n", [App]),
			ok;
		{error, Reason} ->
			io:format("cannot start dependency: ~s.~n", [App]),
			erlang:error({app_start_failed, App, Reason})
	end.
