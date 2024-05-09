% Demonstration of spawning, message passing and registering processes
%
% Processes exchange any erlang term as messages
% The `receive` construct allows processes to wait for messages
% Each process has its own mailbox/queue, when a process executes receive, the first message in the queue is matched against the first pattern of receive, otherwise second, third, etc. until a pattern is matched.
% If there are no more patterns, we move on to the next message. We repeat the process on the rest of the mailbox. Then we wait for more messages.
% Messages not matched to a pattern are not discarded (likely because you may hotload a new pattern match in and it'll unblock the queue.

-module(pingpong_basic).

-export([start/0, ping/2, pong/0]).

ping(0, PongPID) ->
    PongPID ! finished,
    io:format("ping finished~n", []);
ping(N, PongPID) ->
    PongPID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, PongPID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, PingPID} ->
            io:format("Pong received ping~n", []),
            PingPID ! pong,
            pong()
    end.

start() ->
    % Basic way: using BIF spawn
    PongPID = spawn(pingpong, pong, []),
    spawn(pingpong, ping, [3, PongPID]).
