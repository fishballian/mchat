-module(mchat).

-export([new_channel/0,
        join/3,
        leave/2,
        send/3]).

new_channel() ->
    supervisor:start_child(mchat_sup, []).

join(Channel, Name, Pid) ->
    mchat_server:join(Channel, Name, Pid).

leave(Channel, Name) ->
    mchat_server:leave(Channel, Name).

send(Channel, Name, Msg) ->
    mchat_server:send(Channel, Name, Msg).


