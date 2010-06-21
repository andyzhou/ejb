%%ejb service manager
%%receive client command and reply it.
%%
-module(ejb_service_manager).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).
-include("ejb.hrl").

%%API
-export([process_command/2]).


%%procec socket command
process_command(Socket, Data) ->
    %%io:format("~nreceive from ~p, data:~p~n", [Socket, Data]),
    
    {Status, Reply} = analize_command(Socket, Data),
        
    if
        is_atom(Status) ->
            Atom = Status;
        true ->
            Atom = error
    end,
        
    if
        is_list(Reply) ->
            case length(Reply) of
                1 ->
                    Msg = ?XML_HEADER ++ "<ret>" ++ Reply ++ "</ret>" ++ ?XML_BOTTOM;
                _ ->
                    Msg = Reply ++ "\n\r"
            end;
            true ->
                Msg = ?XML_HEADER ++ "<ret>0</ret><reason>Invalid Command</reason>" ++ ?XML_BOTTOM
    end,
            
    {Atom, Msg}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%analize signal command
%%will be record {ok, data} | {error, reason}
analize_command(Socket, Data) ->

    %%command string convert, and remove unuseful characters
    DList = binary_to_list(Data),
    Str = string:strip(DList, both, $\n),
    NewStr = string:strip(Str, both, $\r),
    
    %%analize xml data, get command info
    {XmlObj, _Rest} = xmerl_scan:string(NewStr),
    
    [#xmlText{value=CmdStr}]  = xmerl_xpath:string("//cmd/text()", XmlObj),
    
    if 
        is_list(CmdStr), length(CmdStr) > 0 ->
            Command = CmdStr;
        true ->
            Command = "Invalid"
    end,
        
    Command_atom = list_to_atom(Command),
    
    io:format("~nCommand:~p~n", [Command_atom]),
    
    %%get data list excepted command
    case Command_atom of
        reg ->
            reg_command(Socket, XmlObj);
        
        login ->
            login_command(Socket, XmlObj);
        
        logincheck ->
            login_check_command(Socket, XmlObj);
        
        quit ->
            quit_command(Socket, XmlObj);

        getroster ->
            get_roster_command(Socket, XmlObj);
        
        setpresence ->
            set_presence_command(Socket, XmlObj);
        
        getpresence ->
            get_presence_command(Socket, XmlObj);
        
        getpresences ->
            get_presences_command(Socket, XmlObj);
        
        sendmsg ->
            send_msg_command(Socket, XmlObj);
        
        getmsg ->
            get_msg_command(Socket, XmlObj);
        
        subscribe->
            subscribe_command(Socket, XmlObj);
        
        _->
            {error, "0"}
    end.


%%subscribe signal roster
subscribe_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=Who}]   = xmerl_xpath:string("//who/text()", XmlObj),
    %%[#xmlText{value=_Msg}]   = xmerl_xpath:string("//msg/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            if 
                is_list(Who), length(Who) >= ?AJABBERD_USER_MIN_LEN, length(Who) =< ?AJABBERD_USER_MAX_LEN ->

                    %%call ejb subscribe
                    Reply = ejb_serv:subscribe(User, Who),
                    {ok, Reply};
                
                true ->
                    {error, "0"}
            end;
        
        true ->
            {error, "0"}
    end.
    

%%get message command
get_msg_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=From}]  = xmerl_xpath:string("//from/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            if 
                is_list(From), length(From) >= ?AJABBERD_USER_MIN_LEN, length(From) =< ?AJABBERD_USER_MAX_LEN ->
            
                    %%call ejb get message
                    Reply = ejb_serv:get_msg(User, From),
                    {ok, Reply};
            
                true ->
                    {error, "0"}
            end;
        
        true ->
            {error, "0"}
    end.

%%send message command
send_msg_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=To}]  = xmerl_xpath:string("//to/text()", XmlObj),
    [#xmlText{value=Msg}]  = xmerl_xpath:string("//msg/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            if is_list(To), length(To) >= ?AJABBERD_USER_MIN_LEN, length(To) =< ?AJABBERD_USER_MAX_LEN ->
                
                if is_list(Msg), length(Msg) > 0 ->
            
                    %%call ejb send message
                    ejb_serv:send_msg(User, To, Msg),        
                    {ok, "1"};
                
                true ->
                    {error, "0"}
                end;
        
            true ->
                {error, "0"}
            
            end;
        
        true ->
            {error, "0"}
    end.


%%get client presence command
get_presence_command(_Socket, XmlObj) ->
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            %%call ejb get presence
            Reply = ejb_serv:get_presence(User),        
            {ok, Reply};
        
        true ->
            {error, "0"}
    end.
    

%%get multi clients presences command
get_presences_command(_Socket, XmlObj) ->
    [#xmlText{value=UserListStr}]  = xmerl_xpath:string("//userlist/text()", XmlObj),
    UserList = string:tokens(UserListStr, ","),    
    %%call ejb get presences
    Reply = ejb_serv:get_presences(UserList),    
    {ok, Reply}.
    
    
%%set client presence command
set_presence_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=Presence}]  = xmerl_xpath:string("//presence/text()", XmlObj),
    [#xmlText{value=Status}]  = xmerl_xpath:string("//status/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            if 
                is_list(Status), length(Status) > 0 ->
    
                    %%call ejb set presence
                    ejb_serv:set_presence(User, Presence, Status),        
                    {ok, "1"};
            
                true ->
                    {error, "0"}
            end;
            
        
        true ->
            {error, "0"}
    end.



%%get client rosters command
get_roster_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
        
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
            
            %%call ejb get rosters   
            Reply = ejb_serv:get_rosters(User),
            {ok, Reply};
        
        true ->
            {error, "0"}
    end.


%%login check command
%%0:not logined 1:logined
login_check_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
        
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
            
            %%call ejb login module       
            Reply = ejb_serv:get_login_status(User),
        
            {ok, Reply};
             
        true ->
            {error, "0"}
    end.


%%login command
%%Data -> list()
login_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=Password}]  = xmerl_xpath:string("//passwd/text()", XmlObj),
       
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
    
            %%call ejb login module       
            ejb_serv:login(User, Password),
                
            %%only for return, because im object will be spawn as new process...
            %%we can call ejb_serv:login_check get login status
            {ok, "1"};

        true ->
            {error, "0"}
    end.


%%register command
%%-spec reg_command(Socket, Xml) -> tuple
reg_command(_Socket, XmlObj) ->

    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    [#xmlText{value=Password}]  = xmerl_xpath:string("//passwd/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->
            %%call ejb register module
            Reply = ejb_serv:reg(User, Password),
            {ok, Reply};
        
        true ->
            {error, "0"}
    end.

%%quit command
quit_command(_Socket, XmlObj) ->
    
    [#xmlText{value=User}]  = xmerl_xpath:string("//username/text()", XmlObj),
    
    if
        is_list(User), length(User) >= ?AJABBERD_USER_MIN_LEN, length(User) =< ?AJABBERD_USER_MAX_LEN ->

            %%call ejb quit module       
            Reply = ejb_serv:quit(User),        
            {ok, Reply};

        true ->
            {error, "0"}
    end.
    