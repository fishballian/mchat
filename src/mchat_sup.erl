%%%-------------------------------------------------------------------
%% @doc mchat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mchat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [{mchat_server, {mchat_server,start_link, []}, transient, brutal_kill, worker, [mchat_server]}] }}.

%%====================================================================
%% Internal functions
%%====================================================================
