%%ejb chat module
%%
-module(ejb_mod_chat).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").


%%API
-export([send_msg/3, save_msg/5, get_msg/3]).

%%get message from local ets table
get_msg(Dict, User, From) ->
    MsgKey = User ++ "_" ++ From ++ "_msg",
    
    {_,_,ChatTab} = Dict,
    Rec = ets:lookup(ChatTab, MsgKey),    
    %%io:format("~nRec11111111111:~p~n", [Rec]),
    
    %%reformat chat message as xml stream
    ChatXmlMsg = convert_chat_msg_to_xml(Rec, []),
    ChatXmlList = ?XML_HEADER ++ "<msgs>" ++ ChatXmlMsg ++ "</msgs>" ++ ?XML_BOTTOM,
    
    %%delete received message from ets table
    ets:delete(ChatTab, MsgKey),    
    {ok, ChatXmlList}.


%%send message to remote client
send_msg(MySession, To, Msg) ->
    MsgNew = exmpp_message:make_chat(?NS_JABBER_CLIENT, Msg),
    ToJID  = To ++ "@" ++ ?EJABBERD_DOMAIN,
    MsgDst = exmpp_xml:set_attribute(MsgNew, to, ToJID),
    exmpp_session:send_packet(MySession, MsgDst).


%%save received message in local ets table
%%client to client
save_msg(_MySession, User, FromJID, Packet, Dict) ->
    From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
    MsgBody = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, body)),
    
    %%io:format("~n~p~n", [Packet]),
    %%io:format("~nbsize:~p~n", [size(MsgBody)]),
    
    if
        is_binary(MsgBody), size(MsgBody) >= 1 ->            
            %%io:format("~nfrom:~p, msg:~p~n", [FromJID, MsgBody]),
            From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
            TmpList = string:tokens(From, "@"),
            [FromWho|_T] = TmpList,
        
            %%save message into ets table
            %%Key:User_From_msg
            MsgKey = User ++ "_" ++ FromWho ++"_msg",
        
            {_,_,ChatTab} = Dict,
            ets:insert(ChatTab, {MsgKey, MsgBody}),
            
            %%Rec = ets:lookup(ChatTab, MsgKey),
            %%io:format("~nA:~p, len:~p~n", [Rec, length(Rec)]);
            ok;
        
        true ->
            void
    end.



%%%%%%



%%%%%%%%%%%%%%%%%%%%%%
%%convert chat message into xml format
convert_chat_msg_to_xml([H|T], XmlList) when is_tuple(H) ->
    TmpMsg = tuple_to_list(H),
    [_|T1] = TmpMsg,
    [R] = T1,
    Msg = binary_check(R),
    %%io:format("~nSigMsg:~p~n", [Msg]),
    
    %%new xml
    SigXml = "<chat><![CDATA[" ++ Msg ++ "]]></chat>",
    NewXmlList = XmlList ++ SigXml,
     
    convert_chat_msg_to_xml(T, NewXmlList);
convert_chat_msg_to_xml([], XmlList) ->
    XmlList.


%%check variable is binary
binary_check(X) when is_binary(X) ->
    binary_to_list(X);
binary_check(X) ->
    X.
