%%ejb supervisor server
%%

-module(ejb_sup_server).
-vsn(0.1).
-author("Andy Chow <diudiu8848@163.com>").
-behaviour(supervisor).

%%API
-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).
    
init(Args) ->
    [_JBServCfg, RobotServCfg, SockServCfg|_T] = Args,    
    
    ChildGenServ = {ejb_serv, {ejb_serv, start, []}, permanent, 2000, worker, [ejb_serv]},
    ChildRobotServ = {ejb_robot_serv, {ejb_robot_serv, start, [{local, RobotServCfg}]}, permanent, 2000, worker, [ejb_robot_serv]},
    ChildSockServ = {ejb_socket_server, {ejb_socket_server, start, [{local, SockServCfg}]}, permanent, 2000, worker, [ejb_socket_server]},
    
    {ok, {{one_for_one, 3, 10}, [ChildGenServ, ChildRobotServ, ChildSockServ]}}.
