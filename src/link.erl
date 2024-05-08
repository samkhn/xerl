-module(link).

-export([start/1, ping/2, pong/0]).

ping(0, _) ->
    exit(ping);
ping(N, PongPID) ->
    link(PongPID),
    ping_impl(N, PongPID).

ping_impl(0, _) ->
    exit(ping);
ping_impl(N, PongPID) ->
    PongPID ! {ping, self()},
    receive
	pong ->
	    io:format("Ping received pong~n",[])
    end,
    ping_impl(N - 1, PongPID).

pong() ->
    process_flag(trap_exit, true),
    pong_impl().

pong_impl() ->
    receive
	{ping, PingPID} ->
	    io:format("Pong received ping~n", []),
	    PingPID ! pong,
	    pong();
	{'EXIT', From, Reason} ->
	    io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
    end.


start(PingNode) ->
    PongPID = spawn(link, pong, []),
    spawn(PingNode, link, ping, [3, PongPID]).
