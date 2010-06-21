%%ejb core module
%%
-module(ejb_serv).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).
-behaviour(gen_server).

-include("ejb.hrl").

%%API
-export([reg/2]).
-export([start/0, stop/0, login/2, get_login_status/1]).
-export([quit/1, get_rosters/1, get_presence/1, get_presences/1, set_presence/3]).
-export([send_msg/3, get_msg/2, subscribe/2]).



%%Internal API
-export([exmpp_init/3, client_login/3, core_loop/3]).

%%gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%%API%%%%%%%%%%
start() ->
    %%start gen server
    case gen_server:start({local, ?EJB_MOD_SERVER}, ?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Other ->
            Other
    end.

stop() ->
    gen_server:call(?EJB_MOD_SERVER, stop).

reg(User, Password) ->
    gen_server:call(?EJB_MOD_SERVER, {reg_req, User, Password}).

login(User, Password) ->
    gen_server:call(?EJB_MOD_SERVER, {login_req, User, Password}).

%%login status will be storage in local ets table
%%user->status
get_login_status(User) ->
    gen_server:call(?EJB_MOD_SERVER, {get_login_status_req, User}).
    
quit(User) ->
    gen_server:call(?EJB_MOD_SERVER, {quit_req, User}).
    
get_rosters(User) ->
    gen_server:call(?EJB_MOD_SERVER, {get_rosters_req, User}).

get_presence(User) ->
    gen_server:call(?EJB_MOD_SERVER, {get_presence_req, User}).
    
get_presences(UserList) ->
    gen_server:call(?EJB_MOD_SERVER, {get_presences_req, UserList}).

set_presence(User, Presence, Status) ->
    gen_server:call(?EJB_MOD_SERVER, {set_presence_req, User, Presence, Status}).
    
send_msg(User, To, Msg) ->
    gen_server:call(?EJB_MOD_SERVER, {send_msg_req, User, To, Msg}).
    
get_msg(User, From) ->
    gen_server:call(?EJB_MOD_SERVER, {get_msg_req, User, From}).
    
subscribe(User, Who) ->
    gen_server:call(?EJB_MOD_SERVER, {subscribe_req, User, Who}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%handle call of request
%%tmp data will be storage in ets table
%%Dict:ets table id

%%subscribe
handle_call({subscribe_req, User, Who}, _From, Dict) ->

    case client_check(Dict, User) of
        {ok, _} ->
            Atom_user = list_to_atom(User),
            Atom_user ! {send_subscribe, Who},
            ReplyStr = "1";
        {error, _} ->
            ReplyStr = "0"
    end,
    
    {reply, ReplyStr, Dict};
    

%%get presence, include status info
handle_call({get_presence_req, User}, _From, Dict) ->
    
    case client_check(Dict, User) of
        {ok, _} ->
            {ok, ReplyStr} = ejb_mod_presence:get_presence(User, Dict);
        {error, _} ->
            ReplyStr = ?XML_HEADER ++ "<ret>0</ret>" ++ ?XML_BOTTOM
    end,
    {reply, ReplyStr, Dict};
    

%%get presences, support multi clients
handle_call({get_presences_req, UserList}, _From, Dict) ->
    ReplyStr = ejb_mod_presence:get_presences(UserList, Dict),
    {reply, ReplyStr, Dict};


%%set presence
handle_call({set_presence_req, User, Presence, Status}, _From, Dict) ->
    
    case client_check(Dict, User) of
        {ok, _} ->
            Atom_user = list_to_atom(User),
            Atom_user ! {set_presence, Presence, Status},    
            ReplyStr = "1";
        {error, _} ->
            ReplyStr = "0"
    end,
       
    {reply, ReplyStr, Dict};
    
    
%%send message to remote client
handle_call({send_msg_req, User, To, Msg}, _From, Dict) ->
    
    case client_check(Dict, User) of
        {ok, _} ->
            Atom_user = list_to_atom(User),
            Atom_user ! {send_msg, To, Msg},    
            ReplyStr = "1";
        {error, _} ->
            ReplyStr = "0"
    end,
    
    {reply, ReplyStr, Dict};


%%get message 
handle_call({get_msg_req, User, FromWho}, _From, Dict) ->
        
    case client_check(Dict, User) of
        {ok, _} ->
            {ok, ReplyStr} = ejb_mod_chat:get_msg(Dict, User, FromWho);
        {error, _} ->
            ReplyStr = ?XML_HEADER ++ "<ret>0</ret>" ++ ?XML_BOTTOM
    end,
    
    {reply, ReplyStr, Dict};


%%get rosters
handle_call({get_rosters_req, User}, _From, Dict) ->

    case client_check(Dict, User) of
        {ok, _} ->
            {ok, ReplyStr} = ejb_mod_roster:get_roster_list(User, Dict);
        {error, _} ->
            ReplyStr = ?XML_HEADER ++ "<ret>0</ret>" ++ ?XML_BOTTOM
    end,
    
    {reply, ReplyStr, Dict};
    

%%register
handle_call({reg_req, User, Password}, _From, Dict) ->

    %%io:format("~nregister_req...~n"),    
    {ok, ReplyStr} = ejb_mod_register:reg(User, Password),
    {reply, ReplyStr, Dict};


%%login 
handle_call({login_req, User, Password}, _From, Dict) ->

    case client_check(Dict, User) of
        {ok, _} ->
            Reply = "1";
        {error, _} ->
            %%not logined            
            %%check current client login status from ets table
            %%User->status, 0:login failed 1:logined, null:not login
            Pid = spawn(?MODULE, client_login, [User, Password, Dict]),
            Atom = list_to_atom(User),
            register(Atom, Pid),
            Reply = "1"
    end,

    {reply, Reply, Dict};

%%get login status
handle_call({get_login_status_req, User}, _From, Dict) ->

    %%{UserTab, _, _} = Dict,    
    case client_check(Dict, User) of
        {ok, _} ->
            ReplyStr = "1";
        {error, _} ->
            ReplyStr = "0"
    end,
    
    {reply, ReplyStr, Dict};


%%quit
handle_call({quit_req, User}, _From, Dict) ->
    %%send quit signal to current client process
    Atom_user = list_to_atom(User),
    
    %%delete client login status in ets table
    {UserTab,_,_} = Dict,
    
    case client_check(Dict, User) of
        {ok, _} ->
            Atom_user ! stop,
            ets:delete(UserTab, User),
            ReplyStr = "1";
        {error, _} ->
            ReplyStr = "1"
    end,
    
    {reply, ReplyStr, Dict};
    

handle_call(stop, _From, State) ->
    {stop, normal, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%%%%%%%%%%%%%
%%im client login
client_login(User, Password, Dict) ->
    
    %%process_flag(trap_exit, true),
    if
        is_list(User), is_list(Password) ->         
            exmpp_init(User, Password, Dict),
            {ok, "1"};
        true ->
            Reason = "Invalid user name or password.",
            {error, Reason}
    end.


%%exmpp internal init
exmpp_init(User, Password, Dict) ->
    
    application:start(exmpp),
    
    %%start xmpp session
    MySession = exmpp_session:start(),
    
    %%create xmpp jid
    MyJID = exmpp_jid:make(User, ?EJABBERD_DOMAIN, random),
    
    %%create new session with basic author
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    
    %%connect ejabberd server by TCP
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, ?AJABBERD_SERVER, ?EJABBERD_PORT),
    
    %%try login
    try exmpp_session:login(MySession) catch 
        {auth_error, 'not-authorized'} ->
            exmpp_session:stop(MySession),
            io:format("~nlogin failed!~n"),
            {error, "login failed"};
        _->
            io:format("~nlogin success!~n")
    end,
    
    %%set current client login status in internal ets table
    %%ets:delete(Dict, User),
    {UserTab, RosterTab, _} = Dict, 
    ets:insert(UserTab, {User, 1}),
    %%Reply = ets:lookup(UserTab, User),
    
    %%send init presence status
    exmpp_session:send_packet(MySession,
                              exmpp_presence:set_status(
                              exmpp_presence:available(), "Chat Ready")),
                  
    %%first login, get roster info from im server
    ejb_mod_roster:get_roster_cmd(MySession),
    
    %%add self into roster table
    StatusInfo = "Chat Ready",
    ShowInfo = "online",
    PriorityInfo = "8",%%1~8
    
    io:format("~n...........Who:~p~n", [User]),
    
    ets:insert(RosterTab, {User, {StatusInfo, ShowInfo, PriorityInfo}}),
    
    %%begin core loop
    core_loop(User, MySession, Dict).
    
    
%%core im mod loop
core_loop(User, MySession, Dict) ->
    receive
        %%quit current im client
        stop ->
            {UserTab,RosterTab,_} = Dict,
            ets:delete(UserTab, User),
            ets:delete(RosterTab, User),
            exmpp_session:stop(MySession);
        
        %%subscribe new roster
        {send_subscribe, Who} ->
            io:format("~nsend_subscribe:~p~n", [Who]),
            ejb_mod_subscribe:subscribe_roster(MySession, Who),
            core_loop(User, MySession, Dict);
        
        %%send message to remote client
        {send_msg, To, Msg} ->
            ejb_mod_chat:send_msg(MySession, To, Msg),
            core_loop(User, MySession, Dict);
        
        %%set presence
        {set_presence, Presence, Status} ->
            ejb_mod_presence:set_presence(MySession, Presence, Status),
            core_loop(User, MySession, Dict);
        
        %%receive and resolve subscribe xml packet
        %%if current logined client is ROBOT, it will be subscribed automatic!
        _Recored = #received_packet{packet_type=presence, type_attr="subscribe", from=FromJID, raw_packet=_Packet} ->
            io:format("~nsubscribe....~n"),
            ejb_mod_subscribe:subscribe_process(MySession, FromJID),
            core_loop(User, MySession, Dict);
        
        
        %%receive and resolve subscribed xml packet
        _Recored = #received_packet{packet_type=presence, type_attr="subscribed", from=FromJID, raw_packet=Packet} ->
            io:format("~nsubscribed....~n"),
            ejb_mod_subscribe:subscribed_process(MySession, User, FromJID, Packet, Dict),
            core_loop(User, MySession, Dict);
        
        
        %%receive and resolve roster presence xml packet <available>
        _Recored = #received_packet{packet_type=presence, type_attr="available", from=FromJID, raw_packet=Packet} ->
            io:format("~nsync_presence1....~n"),
            ejb_mod_presence:sync_presence(MySession, FromJID, Packet, Dict),
            core_loop(User, MySession, Dict);
        
        %%receive and resolve roster presence xml packet <unavailable>
        _Recored = #received_packet{packet_type=presence, type_attr="unavailable", from=FromJID, raw_packet=Packet} ->
            io:format("~nsync_presence2....~n"),
            ejb_mod_presence:sync_presence(MySession, FromJID, Packet, Dict),
            core_loop(User, MySession, Dict);
        
        
        %%receive and resolve chat message xml packet
        %%type_attr->normal,chat?
        %%if ROBOT receive message, will be processed by special method.
        _Recored = #received_packet{packet_type=message, type_attr="chat", from = FromJID, raw_packet=Packet} ->
    
            if
                User =:= ?AJABBERD_ROBOT_NAME ->
                    ejb_robot:process_chat(MySession, FromJID, Packet, Dict);
                true ->
                    ejb_mod_chat:save_msg(MySession, User, FromJID, Packet, Dict)
            end,
        
            core_loop(User, MySession, Dict);
    
        
        %%receive and resolve roster xml packet
        _Recored = #received_packet{packet_type=iq, raw_packet=Packet} ->
            io:format("~n............roster.........~n"),
            %%analize and store roster info
            ejb_mod_roster:store_roster_info(User, Packet, Dict),
            core_loop(User, MySession, Dict);
        
        _Any ->
            core_loop(User, MySession, Dict)
    end.


%%%%%%%%%%%%%

%%check client is logined, return true or false;
client_check(Dict, UserName) -> 
    {UserTab, _, _} = Dict,
    case ets:lookup(UserTab, UserName) of
        [_Rec] ->
            {ok, 1};
        [] ->
            {error, 0}
    end.
    
%%init internal ets table for storage tmp variables
init([]) ->
    process_flag(trap_exit, true),
    
    %%create internal data storage ets tables
    io:format("~ninit ets table...~n"),
    UserTab = ets:new(ejb_user, [public, ordered_set, {write_concurrency, true}]),%%ordered set
    ChatTab = ets:new(ejb_chat, [public, duplicate_bag, {write_concurrency, true}]),%%duplicate bag
    RosterTab = ets:new(ejb_roster, [public, ordered_set, {write_concurrency, true}]),%%ordered set
    
    %%%
    Tables = {UserTab, RosterTab, ChatTab},  
    {ok, Tables}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
  ok.
    
handle_info(_Info, State) ->   
    {noreply, State}.  
    
code_change(_OldVsn, State, _Extra) ->   
    {ok, State}.  
