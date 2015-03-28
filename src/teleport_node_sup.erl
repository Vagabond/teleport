-module(teleport_node_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Node) ->
  supervisor:start_link(?MODULE, [Node]).

init([Node]) ->
  Name = teleport:name_for_node(Node),
  PoolArgs = [{name, {local, Name}}, {size, 6}, {max_overflow, 5}, {worker_module, teleport_node_worker}],
  WorkerArgs = [{node, Node}],
  {ok, {{rest_for_one, 10, 10}, [
                                 {teleport_node_watcher, {teleport_node_watcher, start_link, [Node]},
                                  transient, 5000, worker, [teleport_node_watcher]},
                                {Name, {poolboy, start_link, [PoolArgs, WorkerArgs]},
                                 transient, 5000, worker, [poolboy]}]}}.




