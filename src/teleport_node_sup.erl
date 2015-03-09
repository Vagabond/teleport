-module(teleport_node_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Node) ->
  supervisor:start_link(?MODULE, [Node]).

init([Node]) ->
  Name = teleporter:name_for_node(Node),
  PoolArgs = [{name, {local, Name}}],
  WorkerArgs = [{node, Node}],
  {ok, {{one_for_one, 10, 10}, [
                                {Name, {poolboy, start_link, [PoolArgs, WorkerArgs]},
                                 transient, 5000, worker, [poolboy]}]}}.
