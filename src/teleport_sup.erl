-module(teleport_sup).
-behaviour(supervisor).

-export([start_link/0, add_node/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_node(Node) ->
  supervisor:start_child(?MODULE, {Node, {teleport_node_sup, start_link, [Node]},
                                      transient, 5000, worker, [teleport_node_sup]}).

init([]) ->
  _ = try ets:new(teleport_workers, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]) of
        _Result ->
          ok
      catch
        error:badarg ->
          ok
      end,
  {ok, {{one_for_one, 10, 10}, [
                                {teleport_listen_server,
                                 {teleport_listen_server, start_link, []},
                                 permanent, 5000, worker, [teleport_listen_server]},
                                {teleport_socket_sup,
                                 {teleport_socket_sup, start_link, []},
                                 permanent, 5000, supervisor, [teleport_socket_sup]}
                               ]}}.
