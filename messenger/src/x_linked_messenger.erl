-module(x_linked_messenger).

-export([start_server/0, server/0, login/1, logoff/0, message/2, client/2]).

server_node() ->
    'messenger@samiur-mbp'.

server() ->
    process_flag(trap_exit, true),
    server([]).

server(UserList) ->
    receive
	{From, login, Name} ->
	    NewUserList = server_login(From, Name, UserList),
	    server(NewUserList);
	{'EXIT', From, _} ->
	    NewUserList = server_logoff(From, UserList),
	    server(NewUserList);
	{From, message_to, To, Message} ->
	    server_transfer(From, To, Message, UserList),
	    io:format("Users logged in are ~p~n", [UserList]),
	    server(UserList)
    end.

start_server() ->
    register(messenger, spawn(x_linked_messenger, server, [])).

server_login(From, Name, UserList) ->
    case lists:keymember(Name, 2, UserList) of
	true ->
	    From ! {messenger, stop, user_exists_at_another_node},
	    UserList;
	false ->
	    From ! {messenger, logged_in},
	    link(From),
	    io:format("~p logged in as ~p~n", [From, Name]),
	    [{From, Name} | UserList]
    end.

server_logoff(From, UserList) ->
    io:format("~p logged off~n", [From]),
    lists:keydelete(From, 1, UserList).

server_transfer(From, To, Message, UserList) ->
    case lists:keysearch(From, 1, UserList) of
	false ->
	    From ! {messenger, stop, you_are_not_logged_in};
	{value, {_, Name}} ->
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

login(Name) ->
    case whereis(messenger_client) of
	undefined ->
	    register(messenger_client, spawn(x_linked_messenger, client, [server_node(), Name]));
	_ ->
	    already_logged_in
    end.

logoff() ->
    messenger_client ! logoff.

message(ToName, Message) ->
    case whereis(messenger_client) of
	undefined ->
	    not_logged_in;
	_ -> messenger_client ! {message_to, ToName, Message},
	     ok
    end.

client(ServerNode, Name) ->
    {messenger, ServerNode} ! {self(), login, Name},
    client_await_server(),
    client(ServerNode).

client_await_server() ->
    receive
	{messenger, stop, Why} ->
	    io:format("~p~n", [Why]),
	    exit(normal);
	{messenger, What} ->
	    io:format("~p~n", [What])
    after 5000 ->
	    io:format("No response from server~n", []),
	    exit(timeout)
    end.

client(ServerNode) ->
    receive
	logoff ->
	    exit(normal);
	{message_to, ToName, Message} ->
	    {messenger, ServerNode} ! {self(), message_to, ToName, Message},
	    client_await_server();
	{message_from, FromName, Message} ->
	    io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(ServerNode).
