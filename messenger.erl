% Example messenger program
%
% Public interface:
% login(Name)
%  One user can login at a time at each node with name Name.
%  If the same username exists in any other node, Name is registered.
% logoff()
%  Logs off anybody at that node
% message(ToName, Message)
%  sends Message to ToName. Error messages if the user is not loggin in or
%  if ToName is not logged in on any node.
%
% One node on the network, named messenger_server, maintains data on who is logged in.
% Each node where a user is logged in runs a process called messenger_client
%
% Protocol between server and client
% To server: {ClientPid, logon, UserName}
% Reply {messenger, stop, user_exists_at_other_node} stops the client
% Reply {messenger, logged_on} means login was successful
%
% To server: {ClientPid, logoff}
% Reply {messenger, loggedoff}
% Running above again to server will have no reply
%
% To server: {ClientPid, message_to, ToName, Message} sends a message
% Reply {messenger, stop, login_required} stops the client
% Reply {messenger, receiver_not_found} returned when no user with that name is logged in
% Reply {messenger, sent} (with no guarentee) the message has been sent
%
% To client: {message_from, Name, Message},
%
% Protocol between "commands" and the client
% Started: messenger:client(ServerNode, Name)
% To client: logoff
% To client: {message_to, ToName, Message}
%
% Config: change the server_node() function to return the name of the node where the messenger server is running

-module(messenger).

-export([
         start_server/0,
         server/1,
         logon/1,
         logoff/0,
         message/2,
         client/2
         ]).

server_node() ->
    messenger@devbig047.

server(UserList) ->
    receive
        {From, logon, Name} ->
            NewUserList = server_logon(From, Name, UserList),
            server(NewUserList);
        {From, logoff} ->
            NewUserList = server_logoff(From, UserList),
            server(NewUserList);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, UserList),
            io:format("list is now: ~p~n", [UserList]),
            server(UserList)
    end.

start_server() ->
    register(messenger, spawn(messenger, server, [[]])).

server_logon(From, Name, UserList) ->
    case lists:keymember(Name, 2, UserList) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            UserList;
        false ->
            From ! {messenger, logged_on},
            [{From, Name} | UserList]
    end.

server_logoff(From, UserList) ->
    lists:keydelete(From, 1, UserList).

server_transfer(From, To, Message, UserList) ->
    case lists:keysearch(From, 1, UserList) of
        false ->
            From ! {messenger, stop, login_required};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, UserList)
    end.
server_transfer(From, Name, To, Message, UserList) ->
    case lists:keysearch(To, 2, UserList) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPID, To}} ->
            ToPID ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.

logon(Name) ->
    case whereis(messenger_client) of
        undefined ->
            register(messenger_client, spawn(messenger, client, [server_node(), Name]));
        _ -> already_logged_on
    end.

logoff() ->
    messenger_client ! logoff.

message(ToName, Message) ->
    case whereis(messenger_client) of
        undefined ->
            not_logged_on;
        _ -> messenger_client ! {message_to, ToName, Message},
             ok
    end.

client(ServerNode, Name) ->
    {messenger, ServerNode} ! {self(), logon, Name},
    await_result(),
    client(ServerNode).

client(ServerNode) ->
    receive
        logoff ->
            {messenger, ServerNode} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, ServerNode} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(ServerNode).

await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->
            io:format("~p~n", [What])
    end.
