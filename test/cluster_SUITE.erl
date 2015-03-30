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
    [start_server(Nodes, N-1) || N <- lists:seq(1, length(Nodes))],
    {ok, _} = ct_cover:add_nodes(Nodes),
    [{nodes, Nodes}|Config].

end_per_testcase(_, _Config) ->
    test_utils:pmap(fun(Node) ->ct_slave:stop(Node) end, ?NODES),
    ok.

all() ->
    [perf, reconnect].

perf(Config) ->
    Nodes = proplists:get_value(nodes, Config),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({send_server, Node}, prime, infinity)
                    end, Nodes),
    Res = test_utils:pmap(fun(Node) ->
                            timer:tc(fun() ->
                                             gen_server:call({send_server, Node}, go, infinity)
                                     end)
                          end, Nodes),
    ct:pal("disterl result ~p~n", [Res]),
    {Time, _} = lists:unzip(Res),
    ct:pal("disterl avg ~p~n", [(lists:sum(Time)/length(Nodes))/1000000]),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({send_server, Node}, {set_send_method, teleport}, infinity),
                        gen_server:call({send_server, Node}, prime, infinity)
                    end, Nodes),
    Res2 = test_utils:pmap(fun(Node) ->
                            timer:tc(fun() ->
                                             gen_server:call({send_server, Node}, go, infinity)
                                     end)
                          end, Nodes),
    ct:pal("teleport result ~p~n", [Res2]),
    {Time2, _} = lists:unzip(Res2),
    ct:pal("teleport avg ~p~n", [(lists:sum(Time2)/length(Nodes))/1000000]),

    ok.

reconnect(Config) ->
    Nodes = proplists:get_value(nodes, Config),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({send_server, Node}, {set_send_method, teleport}, infinity),
                        gen_server:call({send_server, Node}, prime, infinity)
                    end, Nodes),
    ct_slave:stop(jaguar),
    test_utils:start_node(jaguar, Config, reconnect),
    start_server(Nodes, 0),
    test_utils:pmap(fun(Node) ->
                        gen_server:call({send_server, Node}, go, infinity)
                    end, Nodes),
    ok.

start_server(Nodes, I) ->
    {End, [N|Start]} = lists:split(I, Nodes),
    ct:pal("Node is ~p~n", [N]),
    %%NL = lists:sort(fun(_A, _B) -> crypto:rand_bytes(1) > crypto:rand_bytes(1) end, Start ++ End),
    NL = case I rem 2 == 0 of
             true ->
                 Start++End;
             false ->
                 lists:reverse(Start++End)
         end,
    {ok, _Pid} = rpc:call(N, send_server, start, [NL]),
    %%link(Pid).
    ok.
