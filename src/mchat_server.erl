%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng
%%% @copyright (C) 2016, mc
%%% @doc
%%%
%%% @end
%%% Created : 2016-11-10 09:40:04.385368
%%%-------------------------------------------------------------------
-module(mchat_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
        join/3,
        leave/2,
        send/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(BATCH_LEN, 10).

-record(state, {names = [], pids = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

join(ChannelPid, Name, ClientPid) ->
    gen_server:cast(ChannelPid, {join, Name, ClientPid}).

leave(ChannelPid, Name) ->
    gen_server:cast(ChannelPid, {leave, Name}).

send(ChannelPid, Name, Msg) ->
    gen_server:cast(ChannelPid, {send, Name, Msg}).

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
init([]) ->
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({join, Name, Pid}, State) ->
    #state{names = Names, pids = Pids} = State,
    {Names2, Pids2} = case lists:member(Name, Names) of
                          true ->
                              {Names, Pids};
                          _ ->

                              {[Name | Names], [Pid | Pids]}
                      end,
    {noreply, State#state{names = Names2, pids = Pids2}};
handle_cast({leave, Name}, State) ->
    #state{names = Names, pids = Pids} = State,
    {Names2, Pids2} = do_delete(Name, Names, Pids),
    {noreply, State#state{names = Names2, pids = Pids2}};
handle_cast({send, _Name, Msg}, State) ->
    #state{pids = Pids} = State,
    do_send(Pids, Msg),
    {noreply, State};
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
do_send(Pids, Msg) ->
    do_send(Pids, Msg, [], 0).

do_send([], Msg, Acc, _Len) ->
    do_send2(Acc, Msg);
do_send(Pids, Msg, Acc, Len) when Len >= ?BATCH_LEN ->
    do_send2(Acc, Msg),
    do_send(Pids, Msg, [], 0);
do_send([H | T], Msg, Acc, Len) ->
    do_send(T, Msg, [H | Acc], Len + 1).

do_send2(Pids, Msg) ->
    erlang:spawn(fun() -> [Pid ! Msg || Pid <- Pids] end).


do_delete(Name, Names, Pids) ->
    do_delete(Name, Names, Pids, [], []).

do_delete(_Name, [], [], NameAcc, PidAcc) ->
    {NameAcc, PidAcc};
do_delete(Name, [Name | T], [_Pid | T2], NameAcc, PidAcc) ->
    {T ++ NameAcc, T2 ++ PidAcc};
do_delete(Name, [NameT | T], [PidT | T2], NameAcc, PidAcc) ->
    do_delete(Name, T, T2, [NameT | NameAcc], [PidT | PidAcc]).
