-module(hello_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  ChildSpec = [#{id => hello_server,
                start => {hello_server, start_link, []}}],
  {ok, {#{}, ChildSpec}}.
