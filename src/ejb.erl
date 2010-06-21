%%ejb application server
%%
-module(ejb).
-vsn(0.1).
-author("Andy Chow <diudiu8848@163.com>").
-behaviour(application).

%%API
-export([start/0, stop/0]).
-export([login/2, quit/1]).

%%CallBack
-export([start/2, stop/1]).


%%api
start() ->
    application:start(?MODULE).
    
stop() ->
    application:stop(?MODULE).

%%call back
start(_StartType, _StartArgs) ->
    %%get jabber server setup from application config
    {ok, JBServCfg} = application:get_env(?MODULE, jb_setup),
    {ok, RobotServCfg} = application:get_env(?MODULE, robot_setup),
    {ok, SockServCfg} = application:get_env(?MODULE, sock_setup),
    AppServCfg = [JBServCfg,RobotServCfg,SockServCfg],
    
    case ejb_sup_server:start_link(AppServCfg) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.
    
stop(_State) ->
    ok.

%%login
login(UserName, UserPasswd) ->
    ejb_serv:login(UserName, UserPasswd).
    
%%quit
quit(UserName) ->
    ejb_serv:quit(UserName).
