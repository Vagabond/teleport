-module(send_server).
-behaviour(gen_server).

%% API
-export([start/1, start_link/1, go/0, prime/0, set_send_method/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          nodes = [],
          payload,
          send=disterl
         }).

start(Nodes) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Nodes], []).

start_link(Nodes) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

go() ->
  gen_server:call(?MODULE, go).

prime() ->
  gen_server:call(?MODULE, prime).

set_send_method(disterl) ->
  gen_server:call(?MODULE, {set_send_method, disterl});
set_send_method(teleport) ->
  gen_server:call(?MODULE, {set_send_method, teleport}).

init([Nodes]) ->
  {ok, #state{nodes=Nodes, payload=crypto:rand_bytes(1024)}}.

handle_call({set_send_method, Send}, _From, State) ->
  {reply, ok, State#state{send=Send}};
handle_call(prime, From, State=#state{nodes=Nodes, send=Send}) ->
    [N|Rest] = Nodes,
    send(Send, {?MODULE, N}, {relay, Rest, From, prime}),
    {noreply, State};
handle_call(go, From, State=#state{nodes=Nodes, payload=P, send=Send}) ->
    [N|Rest] = Nodes,
    send(Send, {?MODULE, N}, {relay, Rest++Nodes++Nodes++Nodes++Nodes, From, P}),
    {noreply, State};
handle_call(_Msg, _From, State) ->
  io:format("unhandled call ~p~n", [_Msg]),
  {reply, unknown, State}.

handle_cast({relay, [], From, _}, State) ->
    gen_server:reply(From, done),
    {noreply, State};
handle_cast({relay, [N|Rest], From, P}, State) ->
    gen_server:cast({?MODULE, N}, {relay, Rest, From, P}),
    {noreply, State};
handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

handle_info({relay, [], From, _}, State) ->
    gen_server:reply(From, done),
    {noreply, State};
handle_info({relay, [N|Rest], From, P}, State=#state{send=Send}) ->
    send(Send, {?MODULE, N}, {relay, Rest, From, P}),
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
