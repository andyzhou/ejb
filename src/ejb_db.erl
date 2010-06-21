%%edb db interface
%%
-module(ejb_db).
-vsn(0.1).
-author("Andy Chow <diudiu8848@163.com>").
-include("ejb.hrl").

%%api
-export([init/2, exec/2]).

%%init db connect pools
init(DBPara, Pools) ->
    
    {Mysql_server, Mysql_user, Mysql_passwd, Mysql_db} = DBPara,
    mysql:start_link(e_db_mysql, Mysql_server, Mysql_user, Mysql_passwd, Mysql_db),
    
    %%create db pool
    conn_pool(1, Pools, DBPara),
    
    %%return db object atom
    {ok, e_db_mysql}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%API%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%execute sql segment
exec(DBObj, Sql) ->
    
    Result = mysql:fetch(DBObj, Sql),
    
    %%io:format("~na:~ts~n", [SqlTest]),
    %%io:format("~nb:~ts~n", [Sql]),
    %%io:format("~nResult:~p~n", [Result]),
    
    {ok, Result}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%private functions%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create mysql connect pool
conn_pool(Max, Max, Para) ->
    {Db_serv, Db_user, Db_pwd, Db_name} = Para,
    mysql:connect(e_db_mysql, Db_serv, undefined, Db_user, Db_pwd, Db_name, true);
conn_pool(Min, Max, Para) ->
    {Db_serv, Db_user, Db_pwd, Db_name} = Para,
    mysql:connect(e_db_mysql, Db_serv, undefined, Db_user, Db_pwd, Db_name, true),
    conn_pool(Min + 1, Max, Para).

