
-module(erlcron_timer).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([load_cron_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { config_path = "", tasks = [], jobs_dir = [] } ).

-include("include/erlcron.hrl").

%%%===================================================================
%%% API
%%%===================================================================

load_cron_config() ->
    gen_server:call(?MODULE,load_config).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%-------------------------------------------------------------------- 

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([ConfigPath]) ->
    {ok, #state{ config_path = ConfigPath } }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(load_config,_From,#state{ config_path = Path } = State) -> 
    load_tasks(file:consult(Path)),
    {reply,ok,State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_tasks({ok,[]}) -> [];
load_tasks({error,_}) -> [];
load_tasks({ok,Data}) ->
    lists:foldl( fun(Timer,Acc) ->
        TimeToStart = create_timers(Timer,calendar:local_time()),
        Acc
    end,[],Data).

create_timers({{0,0},{_,_,_}=Time,_,_},{{CY,CM,CD}=Date,_} = CDate) ->
    diff({Date,Time},CDate,{{CY,CM,CD+1},Time});
create_timers({{0,D},{_,_,_}=Time,_,_},{{CY,CM,_},_} = CDate) ->
    diff({{CY,CM,D},Time},CDate,{{CY,CM+1,D},Time});
create_timers({{M,0},{_,_,_}=Time,_,_},{{CY,_,CD},_} = CDate) ->
    diff({{CY,M,CD},Time},CDate,{{CY+1,M,CD},Time});
create_timers({{M,D},{_,_,_}=Time,_,_},{{CY,_,_},_} = CDate) ->
    diff({{CY,M,D},Time},CDate,{{CY+1,M,D},Time});
create_timers(_,_) -> []. 

diff(D1,D2,D3) when D1 =< D2 ->
    count_milliseconds(calendar:time_difference(D2,D3));
diff(D1,D2,_) ->
    count_milliseconds(calendar:time_difference(D2,D1)).

count_milliseconds({Days,{Hr,Min,Sec}} = F) ->
    io:format("~p~n",[F]),
    round((Days*24 + Hr + Min/60 + Sec/3600) * 3600000).
 