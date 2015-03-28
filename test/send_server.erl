-module(send_server).
-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          nodes = [],
          payload
         }).

start(Nodes) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Nodes], []).

start_link(Nodes) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

init([Nodes]) ->
  {ok, #state{nodes=Nodes, payload=crypto:rand_bytes(15*1024*1024)}}.

handle_call(go, From, State=#state{nodes=Nodes, payload=P}) ->
    [N|Rest] = Nodes,
    %gen_server:cast({?MODULE, N}, {relay, Rest, From, P}),
    %{?MODULE, N} ! {relay, Rest, From, P},
    teleport:send({?MODULE, N}, {relay, Rest++Nodes++Nodes, From, P}),
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
handle_info({relay, [N|Rest], From, P}, State) ->
    %{?MODULE, N} ! {relay, Rest, From, P},
    teleport:send({?MODULE, N}, {relay, Rest, From, P}),
    {noreply, State};
handle_info(_Msg, State) ->
  io:format("unhandled info ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
      ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

