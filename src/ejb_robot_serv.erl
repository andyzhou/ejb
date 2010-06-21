%%ejb robot server module
%%
-module(ejb_robot_serv).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).
-behaviour(gen_server).

-include("ejb.hrl").

%%API
-export([start/1, stop/0]).
-export([save_mblog/2]).

%%gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%API%%%%%%%%%%
start(Args) ->
    %%get robot config
    {_Local, RobotCfg} = Args,
    
    %%start gen server
    case gen_server:start({local, ?EJB_MOD_SERVER}, ?MODULE, RobotCfg, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Other ->
            Other
    end.

stop() ->
    gen_server:call(?MODULE, stop).
    

%%save mini blog
save_mblog(FromWho, Msg) ->
    gen_server:call(?EJB_MOD_SERVER, {save_mblog_req, FromWho, Msg}).


%%handle mini blog
handle_call({save_mblog_req, FromWho, Msg}, _From, State) ->
    {ok, Reply} = ejb_robot:save_mblog(FromWho, Msg, State),
    {reply, Reply, State};
%%handle stop    
handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%save mini blog



%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%

%%init
init(Args) ->
    process_flag(trap_exit, true),
    
    io:format("~nRobotServCfg:~p~n", [Args]),
    [DBCfg,RobotCfg|T] = Args,
    
    {Mysql_server, Mysql_user, Mysql_passwd, Mysql_db, Pools} = DBCfg,
    DBPara = {Mysql_server, Mysql_user, Mysql_passwd, Mysql_db},
    
    {ok, DBObj} = ejb_db:init(DBPara, Pools),
    
    %%robot login im server
    {RobotUser, RobotPasswd} = RobotCfg,
    
    io:format("~nrobot logining..."),
    ejb_serv:login(RobotUser, RobotPasswd),
    io:format("ok~n"),
    
    {ok, DBObj}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
