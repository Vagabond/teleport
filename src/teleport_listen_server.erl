-module(teleport_listen_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([loop/2]).

-record(state, {
          socket :: gen_tcp:socket(),
          port :: pos_integer()
         }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Sock} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
  {ok, Port} = inet:port(Sock),
  spawn_link(?MODULE, loop, [self(), Sock]),
  {ok, #state{socket=Sock, port=Port}}.

handle_call(get_port, _From, State) ->
  {reply, {ok, State#state.port}, State};
handle_call(_Msg, _From, State) ->
  io:format("unhandled call ~p~n", [_Msg]),
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  io:format("unhandled cast ~p~n", [_Msg]),
  {noreply, State}.

handle_info(_Msg, State) ->
  io:format("unhandled info ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
      ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

loop(Parent, ListenSock) ->
  {ok, Sock} = gen_tcp:accept(ListenSock),
  case teleport_socket_sup:add_socket(Sock) of
    {ok, Pid} ->
      case gen_tcp:controlling_process(Sock, Pid) of
        ok ->
          ok;
        Error ->
          io:format("error setting controlling_process ~p~n", [Error])
      end;
    Error2 ->
      io:format("error starting socket handler ~p~n", [Error2])
  end,
  loop(Parent, ListenSock).
