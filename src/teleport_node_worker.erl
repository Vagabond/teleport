-module(teleport_node_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          socket :: gen_tcp:socket()
         }).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  Node = proplists:get_value(node, Args),
  case splitnode(Node, longorshort()) of
    {ok, [_Name, Host]} ->
      io:format("host ~p~n", [Host]),
      case inet:getaddr(Host, inet) of
        {ok, IP} ->
          case gen_server:call({teleport_listen_server, Node}, get_port) of
            {ok, Port} ->
              {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 4}, {active, once}]),
              {ok, #state{socket=Socket}};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

handle_call(Request, _From, State) ->
  io:format("unhandled call ~p~n", [Request]),
  {reply, ok, State}.

handle_cast({send, Dest, Msg}, State) ->
  ok = gen_tcp:send(State#state.socket, term_to_binary({send, Dest, Msg})),
  io:format("sent to ~p~n", [inet:peername(State#state.socket)]),
  {noreply, State};
handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

%handle_info({tcp_closed, Socket}, State = #state{socket=Socket}) ->
%  {stop, normal, State};
handle_info(_Info, State) ->
  io:format("unhandled info ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal
longorshort() ->
  case split_node(atom_to_list(node()), $@, []) of
    [nonode, nohost] ->
      none;
    [_Name|Tail] ->
      Host = lists:append(Tail),
      case split_node(Host, $., []) of
        [_] ->
          shortnames;
        L when length(L) > 1 ->
          longnames
      end
  end.


%%% XXX taken from inet_tcp_dist.erl
%% If Node is illegal terminate the connection setup!!
splitnode(_, none) ->
  {error, nodistribution};
splitnode(Node, LongOrShortNames) ->
  case split_node(atom_to_list(Node), $@, []) of
    [Name|Tail] when Tail =/= [] ->
      Host = lists:append(Tail),
      case split_node(Host, $., []) of
        [_] when LongOrShortNames =:= longnames ->
          {error, longnames};
        L when length(L) > 1, LongOrShortNames =:= shortnames ->
          {error, shortnames};
        _ ->
          {ok, [Name, Host]}
      end;
    [_] ->
      {error, illegal}
  end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

