-module(teleport).

-export([send/2, gs_call/3, start/0]).

-export([name_for_node/1]).

start() ->
  application:ensure_all_started(teleport).

send(Process, Message) ->
  Node = get_node(Process),
  case node_addressable(Node) of
    false ->
      {error, nodedown};
    _ ->
      Name = name_for_node(Node),
      do_send(Process, Name, whereis(Name), Message)
  end.

gs_call(Process, Message, Timeout) ->
  Node = get_node(Process),
  case node_addressable(Node) of
    false ->
      exit({nodedown, Node});
    _ ->
      Mref = erlang:monitor(process, Process),
      Name = name_for_node(Node),
      do_send(Process, Name, whereis(Name), {'$gen_call', {self(), Mref}, Message}),
      receive
        {Mref, Reply} ->
          erlang:demonitor(Mref, [flush]),
          {ok, Reply};
        {'DOWN', Mref, _, _, noconnection} ->
          exit({nodedown, Node});
        {'DOWN', Mref, _, _, Reason} ->
          exit(Reason)
      after Timeout ->
              erlang:demonitor(Mref, [flush]),
              exit(timeout)
      end
  end.


name_for_node(Node) ->
  list_to_atom(lists:flatten(io_lib:format("~s_~s", [Node, teleport]))).


node_addressable(Node) ->
  case lists:member(Node, nodes()) of
    true ->
      true;
    _ ->
      pong == net_adm:ping(Node)
  end.

do_send(Process, Name, undefined, Msg) ->
  case teleport_sup:add_node(get_node(Process)) of
    {error, _} = Error ->
      Error;
    {ok, _} ->
      do_send(Process, Name, whereis(Name), Msg)
  end;
do_send(Process, _Name, Pid, Message) ->
  poolboy:transaction(Pid, fun(Worker) ->
                               gen_server:cast(Worker, {send, get_dest(Process), Message})
                           end).

get_node({Name, Node}) when is_atom(Name), is_atom(Node) ->
  Node;
get_node(Pid) when is_pid(Pid) ->
  node(Pid).

get_dest({Name, Node}) when is_atom(Name), is_atom(Node) ->
  Name;
get_dest(Pid) when is_pid(Pid) ->
  Pid.
