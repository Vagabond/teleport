-module(teleport_socket_sup).
-behaviour(supervisor).

-export([start_link/0, add_socket/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_socket(Sock) ->
  supervisor:start_child(?MODULE, [Sock]).

init([]) ->
  {ok, {{simple_one_for_one, 10, 10}, [{call, {teleport_socket, start_link, []},
                                             temporary, brutal_kill, worker, [teleport_socket]}]}}.

