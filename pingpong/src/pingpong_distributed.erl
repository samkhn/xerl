% Distributed ping pong
%
% 0) Create a shared cookie by creating $HOME/.erlang.cookie
% 1) open two terminals
% 2) in terminal A, run $ erl -sname ping
% 3) in terminal B, run $ erl -sname pong
% 4) in terminal B, run $ pingpong_distributed:start_pong().
% 5) in terminal A, run $ pingpong_distributed:start_ping(pong@<other machine name>).
%

-module(pingpong_distributed).

-export([start_ping/1, start_pong/0, ping/2, pong/0]).

ping(0, PongNode) ->
    {pong, PongNode} ! finished,
    io:format("ping finished~n", []);
ping(N, PongNode) ->
    {pong, PongNode} ! {ping, self()},
    receive
        pong ->
            io:format("ping received pong~n", [])
    end,
    ping(N - 1, PongNode).

pong() ->
    receive
        finished ->
            io:format("Pong finished ~n", []);
        {ping, PingPID} ->
            io:format("Pong received ping~n", []),
            PingPID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(distributed_pingpong, pong, [])).

start_ping(PongNode) ->
    spawn(distributed_pingpong, ping, [3, PongNode]).
