%%%-------------------------------------------------------------------
%%% @author Krzysztof Spytkowski
%%% @copyright (C) 2015 kspytkowski
%%% @doc
%%%	Module with the specification of a server handling client requests
%%% @end
%%%-------------------------------------------------------------------
-module(webscraping_srv).
-author("Krzysztof Spytkowski").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(MAX_SCRAPER_DELAY, 4000).
-define(MAX_CLIENT_TIME_HANDLING, 10000).

%% ===================================================================
%% API Function Exports
%% ===================================================================

-export([start_link/0, actual_weather/2]).

%% ===================================================================
%% gen_server Function Exports
%% ===================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, respond_with_collected_data/3]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Asks server about actual weather at given location
%% @end
%%--------------------------------------------------------------------
actual_weather(Province, City) ->
	gen_server:call(?SERVER, {go_scraping, Province, City}, ?MAX_CLIENT_TIME_HANDLING).

%% ===================================================================
%% gen_server Function Definitions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
	io:format("starting server: ~s.~n", [?SERVER]),
	process_flag(trap_exit, true), % to get information about children's death
	{ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({go_scraping, Province, City}, Client, State) ->
	io:format("client (~p) asked about weather.~n", [element(1, Client)]),
	go_scraping(Client, Province, City),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(shutdown, State) ->
	io:format("stopping server: ~w.~n", [?SERVER]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', _CollectorPid, normal}, State) ->
	io:format("collector process ended.~n"),
    {noreply, State};

handle_info(Msg, State) ->
	io:format("~w received unexpected message: ~p.~n", [?SERVER, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	io:format("terminating server: ~s.~n", [?SERVER]),
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
%% Start the process of scraping information from websites: creates 
%% collector of all data sent back from scrapers and asks them 
%% to get to work
%% @end
%%--------------------------------------------------------------------
go_scraping(Client, Province, City) ->
	Scrapers = supervisor:which_children(webscraping_scrapers_sup),
	CollectorPid = spawn_link(?MODULE, respond_with_collected_data, [Client, orddict:new(), orddict:size(Scrapers)]),
	ask_scrapers(Scrapers, CollectorPid, Province, City).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls scrapers to get to work
%% @end
%%--------------------------------------------------------------------
ask_scrapers([], _CollectorPid, _Province, _City) -> 
	void;
ask_scrapers([{_, ScraperPid, _, _} | T], CollectorPid, Province, City) ->
	gen_server:cast(ScraperPid, {check_condition, Province, City, CollectorPid}),
	ask_scrapers(T, CollectorPid, Province, City).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Collects all data sent back from scrapers and forwards them 
%% to the client
%% @end
%%--------------------------------------------------------------------
respond_with_collected_data(Client, Weather, LeftScrapersAmount) ->
	receive 
		{value, Attribute, Value} ->
			NewWeather = orddict:store(Attribute, Value, Weather),
			case LeftScrapersAmount of
				1 ->
					io:format("collected weather information sent to client (~p).~n", [element(1, Client)]),
					gen_server:reply(Client, orddict:to_list(NewWeather));
				_ ->
					respond_with_collected_data(Client, NewWeather, LeftScrapersAmount - 1)
			end;
		{bad_location, Province, City} ->
			io:format("information about error sent to client (~p).~n", [element(1, Client)]),
			gen_server:reply(Client, "Blednie podana lokalizacja: " ++ Province ++ ", " ++ City)
	after 
		?MAX_SCRAPER_DELAY -> 
			io:format("collected weather information sent to client (~p).~n", [element(1, Client)]),
			gen_server:reply(Client, orddict:to_list(Weather))
	end.
