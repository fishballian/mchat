%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng
%%% @copyright (C) 2016, mc
%%% @doc
%%%
%%% @end
%%% Created : 2016-11-10 16:13:54.252474
%%%-------------------------------------------------------------------
-module(mchat_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         %% TODO: test case names go here
         chat/1
        ]).

-include_lib("common_test/include/ct.hrl").


all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     chat
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        %% TODO: group definitions here e.g.
        %% {crud, [], [
        %%          t_create_resource,
        %%          t_read_resource,
        %%          t_update_resource,
        %%          t_delete_resource
        %%         ]}

    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->

    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
chat(_Config) ->
    application:start(mchat),
    {ok, P} = mchat:new_channel(),
    mchat:join(P, xx, self()),
    mchat:send(P, xx, foo),
    receive
        {send, foo} ->
            next;
        Msg ->
            ct:pal(Msg),
            exit(error_msg)
    after
        100 ->
            exit(no_foo_reveice)
    end.


