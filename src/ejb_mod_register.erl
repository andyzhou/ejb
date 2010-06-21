%%ejb register module
%%
-module(ejb_mod_register).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").

%%API
-export([reg/2]).


%%register new account
reg(User, Password) ->

    application:start(exmpp),

    io:format("~n--------------reg-------------~n"),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    
    %% Create XMPP ID (Session Key):
    %%MyJID = exmpp_jid:make("echo", "localhost", random),
    MyJID = exmpp_jid:make(User, ?EJABBERD_DOMAIN, random),
    
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, ?AJABBERD_SERVER, ?EJABBERD_PORT),
    
    try exmpp_session:login(MySession)
    catch 
        throw:{auth_error, 'not-authorized'} ->
        io:format("begin register~n",[]),
        exmpp_session:register_account(MySession, Password)
    end,
    
    exmpp_session:stop(MySession),
    
    Reply = ?XML_HEADER ++ "<ret>1</ret>" ++ ?XML_BOTTOM,
    {ok, Reply}.
    
