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
  io:format("unhandled call ~p~n", [_Msg]),
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

handle_info({tcp, Socket, Payload}, State = #state{socket=Socket}) ->
  inet:setopts(Socket, [{active, once}]),
  %% TODO use safe deserialization and catch exceptions
  case binary_to_term(Payload) of
    {send, Dest, Msg} when is_pid(Dest); is_atom(Dest) ->
      catch(Dest ! Msg);
    Other ->
      io:format("unhandled message ~p~n", [Other])
  end,
  {noreply, State};
handle_info(_Msg, State) ->
  io:format("unhandled info ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
      ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

