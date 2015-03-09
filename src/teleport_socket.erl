-module(teleport_socket).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          socket :: gen_tcp:socket()
         }).

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
  ok = inet:setopts(Socket, [{active, once}]),
  {ok, #state{socket = Socket}}.

handle_call(_Msg, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
      ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

