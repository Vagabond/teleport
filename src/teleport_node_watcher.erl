-module(teleport_node_watcher).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          node :: atom()
         }).

start_link(Node) ->
  gen_server:start_link(?MODULE, [Node], []).

init([Node]) ->
  io:format("started node watcher for ~p~n", [Node]),
  erlang:monitor_node(Node, true),
  {ok, #state{node=Node}}.

handle_call(_Msg, _From, State) ->
  io:format("unhandled call ~p~n", [_Msg]),
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

handle_info({nodedown, Node}, State = #state{node=Node}) ->
  io:format("stopping~n"),
  supervisor:terminate_child(teleport_sup, Node),
  supervisor:delete_child(teleport_sup, Node),
  {stop, normal, State};
handle_info(_Msg, State) ->
  io:format("unhandled info ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

