-module(send_server).
-behaviour(gen_server).

%% API
-export([start/2, start_link/2, go/1, prime/1, set_send_method/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          nodes = [],
          payload,
          send=disterl,
          i=0
         }).

start(I, Nodes) ->
  gen_server:start({local, make_name(I)}, ?MODULE, [I, Nodes], []).

start_link(I, Nodes) ->
  gen_server:start_link({local, make_name(I)}, ?MODULE, [I, Nodes], []).

go(I) ->
  gen_server:call(make_name(I), go).

prime(I) ->
  gen_server:call(make_name(I), prime).

set_send_method(I, disterl) ->
  gen_server:call(make_name(I), {set_send_method, disterl});
set_send_method(I, teleport) ->
  gen_server:call(make_name(I), {set_send_method, teleport}).

init([I, Nodes]) ->
  {ok, #state{nodes=Nodes, i=I, payload=crypto:rand_bytes(10*1024*1024)}}.

handle_call({set_send_method, Send}, _From, State) ->
  {reply, ok, State#state{send=Send}};
handle_call(prime, From, State=#state{nodes=Nodes, send=Send}) ->
    [N|Rest] = Nodes,
    send(Send, {make_name(State#state.i), N}, {relay, Rest, From, prime}),
    {noreply, State};
handle_call(go, From, State=#state{nodes=Nodes, payload=P, send=Send}) ->
    [N|Rest] = Nodes,
    send(Send, {make_name(State#state.i), N}, {relay, Rest, From, P}),
    {noreply, State};
handle_call(_Msg, _From, State) ->
  io:format("unhandled call ~p~n", [_Msg]),
  {reply, unknown, State}.

handle_cast({relay, [], From, _}, State) ->
    gen_server:reply(From, done),
    {noreply, State};
handle_cast({relay, [N|Rest], From, P}, State) ->
    gen_server:cast({make_name(State#state.i), N}, {relay, Rest, From, P}),
    {noreply, State};
handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

handle_info({relay, [], From, _}, State) ->
    gen_server:reply(From, done),
    {noreply, State};
handle_info({relay, [N|Rest], From, P}, State=#state{send=Send}) ->
    send(Send, {make_name(State#state.i), N}, {relay, Rest, From, P}),
    {noreply, State};
handle_info(_Msg, State) ->
  io:format("unhandled info ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
      ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(disterl, Dest, Msg) ->
  Dest ! Msg;
send(teleport, Dest, Msg) ->
  teleport:send(Dest, Msg).

make_name(I) ->
  list_to_atom(atom_to_list(?MODULE) ++"_" ++integer_to_list(I)).
