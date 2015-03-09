-module(teleport).

-export([send/3]).

-export([name_for_node/1]).

send(Dest, Node, Message) ->
  case node_addressable(Node) of
    false ->
      {error, nodedown};
    _ ->
      Name = name_for_node(Node),
      do_send(Node, Name, Dest, whereis(Name), Message)
  end.

name_for_node(Node) ->
  list_to_atom(lists:flatten(io_lib:format("~s_~s", [Node, teleporter]))).


node_addressable(Node) ->
  case lists:member(Node, nodes()) of
    true ->
      true;
    _ ->
      pong == net_adm:ping(Node)
  end.

do_send(Dest, Node, Name, undefined, Msg) ->
  case teleport_sup:add_node(Node) of
    {error, _} = Error ->
      Error;
    {ok, _} ->
      do_send(Dest, Node, Name, whereis(Name), Msg)
  end;
do_send(Dest, _Node, _Name, Pid, Message) ->
  poolboy:transaction(Pid, fun(Worker) ->
                               gen_server:cast(Worker, {send, Dest, Message})
                           end).
