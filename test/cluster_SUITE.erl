-module(cluster_SUITE).

-export([
         %% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([
         perf/1,
         reconnect/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(NODES, [jaguar, shadow, thorn, pyros, katana, electra]).

init_per_suite(_Config) ->
    %% this might help, might not...
    os:cmd(os:find_executable("epmd")++" -daemon"),
    {ok, Hostname} = inet:gethostname(),
    case net_kernel:start([list_to_atom("runner@"++Hostname), shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    _Config.

end_per_suite(_Config) ->
    _Config.

init_per_testcase(Case, Config) ->
    Nodes = test_utils:pmap(fun(N) ->
                                    test_utils:start_node(N, Config, Case)
                            end, ?NODES),
    [start_server(Nodes, N-1, 0) || N <- lists:seq(1, length(Nodes))],
    {ok, _} = ct_cover:add_nodes(Nodes),
    [{nodes, Nodes}|Config].

end_per_testcase(_, _Config) ->
    test_utils:pmap(fun(Node) ->ct_slave:stop(Node) end, ?NODES),
    ok.

all() ->
    [perf].%%, reconnect].
  %[reconnect].

perf(Config) ->
    Nodes = proplists:get_value(nodes, Config),
    [[start_server(Nodes, N-1, I) || N <- lists:seq(1, length(Nodes))] || I <- lists:seq(1, 5)],
    Prod = [{X, Y} || X <- Nodes, Y <- lists:seq(0, 5)],
    test_utils:pmap(fun({Node, N}) ->
                        gen_server:call({make_name(N), Node}, prime, infinity)
                    end, Prod),
    Res = test_utils:pmap(fun({Node, N}) ->
                            timer:tc(fun() ->
                                         gen_server:call({make_name(N), Node}, go, infinity)
                                     end)
                          end, Prod),
    ct:pal("disterl result ~p~n", [Res]),
    {Time, _} = lists:unzip(Res),
    ct:pal("disterl avg ~p~n", [(lists:sum(Time)/length(Prod))/1000000]),
    test_utils:pmap(fun({Node, N}) ->
                        %ct:pal("confugiring and priming ~p~p~n", [Node, N]),
                        ok = gen_server:call({make_name(N), Node}, {set_send_method, teleport}, infinity),
                        %ct:pal("configured ~p~p~n", [Node, N]),
                        done = gen_server:call({make_name(N), Node}, prime, infinity),
                        ok
                    end, Prod),
    Res2 = test_utils:pmap(fun({Node, N}) ->
                            timer:tc(fun() ->
                                             case catch(gen_server:call({make_name(N), Node}, go, infinity)) of
                                               {'EXIT', _} ->
                                                 ct:pal("~p~n", [erlang:process_info(self(), [messages])]);
                                               _ ->
                                                 ok
                                             end
                                     end)
                          end, Prod),
    ct:pal("teleport result ~p~n", [Res2]),
    {Time2, _} = lists:unzip(Res2),
    ct:pal("teleport avg ~p~n", [(lists:sum(Time2)/length(Prod))/1000000]),

    ok.

reconnect(Config) ->
    [FirstNode|_] = Nodes = proplists:get_value(nodes, Config),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({make_name(0), Node}, {set_send_method, teleport}, infinity),
                        gen_server:call({make_name(0), Node}, prime, infinity)
                    end, Nodes),
    ct:pal("ready\n"),
    ct_slave:stop(jaguar),
    test_utils:start_node(jaguar, Config, reconnect),
    start_server(Nodes, 0, 0),
    gen_server:call({make_name(0), FirstNode}, {set_send_method, teleport}, infinity),
    gen_server:call({make_name(0), FirstNode}, prime, infinity),
    ct:pal("ready\n"),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({make_name(0), Node}, go, infinity)
                    end, Nodes),
    ok.

start_server(Nodes, I, Number) ->
    {End, [N|Start]} = lists:split(I, Nodes),
    %%NL = lists:sort(fun(_A, _B) -> crypto:rand_bytes(1) > crypto:rand_bytes(1) end, Start ++ End),
    NL = case I rem 2 == 0 of
             true ->
                 Start++End;
             false ->
                 lists:reverse(Start++End)
                 %Start++End
         end,
    {ok, _Pid} = rpc:call(N, send_server, start, [Number, NL]),
    %link(_Pid),
    ok.

make_name(I) ->
  list_to_atom("send_server_" ++integer_to_list(I)).
