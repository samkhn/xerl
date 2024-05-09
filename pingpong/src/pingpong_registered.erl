-module(pingpong_registered).

-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("Ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished ~n", []);
        {ping, PingPID} ->
            io:format("Pong received ping~n", []),
            PingPID ! pong,
            pong()
    end.

start() ->
    % the atom pong is now associated with the Pid returned by spawn
    % we then send messages w/ ! e.g. pong ! {ping, self()}
    register(pong, spawn(registered_pingpong, pong, [])),
    spawn(registered_pingpong, ping, [3]).
