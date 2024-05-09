%% There are better ways to timeout
%% This form of timeouts are useful for handling external events (outside BEAM).
%% Timeout timer starts upon entering {ping, PingPID} expression

-module(pingpong_timeout).

-export([start_ping/1, start_pong/0, ping/2, pong/0]).

-define(MSG_COUNT, 3).

ping(0, _) ->
    io:format("Ping done~n", []);
ping(N, PongNode) ->
    {pong, PongNode} ! {ping, self()},
    receive
	    pong ->
		    io:format("Ping received pong~n", [])
	    end,
    ping(N - 1, PongNode).

pong() ->
    receive
	    {ping, PingPID} ->
		    io:format("Pong received ping~n", []),
		    PingPID ! pong,
		    pong()
	    after 5000 ->
		    io:format("Pong timed out ~n", [])
	    end.

start_ping(PongNode) ->
    spawn(timeout, ping, [?MSG_COUNT, PongNode]).

start_pong() ->
    register(pong, spawn(timeout, pong, [])).
