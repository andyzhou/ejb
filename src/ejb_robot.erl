%%ejb robot
%%
-module(ejb_robot).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").

-export([process_chat/4]).
-export([save_mblog/3]).


%%process client commands
process_chat(MySession, FromJID, Packet, _Dict) ->

    From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
    MsgBody = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, body)),
        
    TT  = exmpp_xml:get_element(Packet, body),
    
    io:format("~nPacket:~p~n", [Packet]),
    io:format("~nTT:~p~n", [TT]),
    io:format("~nNewMsgBody:~p~n", [MsgBody]),
    
    %%from who
    From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
    TmpList = string:tokens(From, "@"),
    [FromWho|_T] = TmpList,
    
    if
        is_binary(MsgBody), size(MsgBody) >= 1 ->
            %%proable is command??
            ejb_robot_serv:save_mblog(FromWho, MsgBody);
    true ->
            %%process as mini blog
            io:format("~nno message~n")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% private functions %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%analize and get command
analize_command(MySession, FromWho, Msg) ->

    Len = size(Msg),
    io:format("~nWho:~p, Msg:~p, size:~p~n", [FromWho, Msg, Len]),
    
    %%show_help(MySession, FromWho).
    %%send_data_to_edb(FromWho, Msg).
    ejb_robot_serv:save_mblog(FromWho, Msg).


%%save mini blog
save_mblog(FromWho, Msg, DBObj) ->
    MsgStr = binary_to_list(Msg),
    [MsgList] = io_lib:format("~ts", [MsgStr]),
    
    io:format("~n~p~n", [Msg]),
    io:format("~n-------------~n"),
    io:format("~n~p~n", [MsgStr]),
    
    SqlFirst = list_to_binary("insert into im_robot_data(who, info) values('" ++ FromWho ++ "','"),
    SqlLast = list_to_binary("'"),
    
    Sql = "insert into im_robot_data(who, info) values('" ++ FromWho ++ "','" ++ MsgList ++ "')",
    {ok, Reply} = ejb_db:exec(DBObj, Sql),
    {ok, Reply}.
    
save_mini_blog(FromWho, Msg) ->
    ok.


%%show help
show_help(MySession, To) ->
    Msg = "help.......",
    ejb_mod_chat:send_msg(MySession, To, Msg).
    

%%edb socket service
send_data_to_edb(FromWho, Msg) ->

    %%MsgStr = binary_to_list(Msg),
    [MsgList] = io_lib:format("~ts", [Msg]),
    io:format("~nmsg:~p~n", [MsgList]),
    XmlData = "<xml><cmd>exec</cmd><sql>insert into im_robot_data(who, info) values('" ++ FromWho ++ "','" ++ MsgList ++ "')</sql></xml>",
    
    {ok, Socket} = gen_tcp:connect(?EDB_SERV_ADDR, ?EDB_SERV_PORT, [binary, {packet, 0}]),
    gen_tcp:send(Socket, list_to_binary(XmlData)),
    recv_edb_msg(Socket),
    gen_tcp:close(Socket).


%%recive edb server response
recv_edb_msg(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      Msg = binary_to_list(Bin),
      io:format("Received msg: ~s~n", [Msg]),
      ok
  end.

    


