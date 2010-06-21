%%ejb presence module
%%
-module(ejb_mod_presence).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").

-export([set_presence/3, get_presence/2, get_presences/2, sync_presence/4]).


%%get multi rosters presence
get_presences(UserList, Dict) ->
    {_,RosterTab,_} = Dict,    
    if
        is_list(UserList), length(UserList) > 0 ->
            PresenceList = get_sig_client_presence(RosterTab, UserList, []),
            RetStr = "<ret>1</ret>";
        true ->
            PresenceList = "",
            RetStr = "<ret>0</ret>"
    end,
    
    ?XML_HEADER ++ RetStr ++ "<reclist>" ++ PresenceList ++ "</reclist>" ++ ?XML_BOTTOM.
    
        
%%process signal client presence (private)
get_sig_client_presence(_RosterTab, [], UserPresenceListStr) ->
    UserPresenceListStr;
get_sig_client_presence(RosterTab, [H|T], UserPresenceListStr) ->
    if
        is_list(H), length(H) > 0 ->
            case ets:lookup(RosterTab, H) of
                [{_,TmpTuple}] ->
                    {ClientSatus,ClientShow,ClientPriority} = TmpTuple, 
                    if
                        is_list(ClientPriority), length(ClientPriority) > 0 ->
                            ClientPriorityStr = ClientPriority;
                        true ->
                            ClientPriorityStr = "1"
                    end,
                    ClientPresenceXml =  "<rec><uname>" ++ H ++ "</uname><presence>" ++ ClientPriorityStr ++ "</presence><status>" ++ ClientSatus ++ "</status><show>" ++ ClientShow ++ "</show></rec>";
                [] ->
                    ClientPresenceXml = ""
            end;
        true ->
            ClientPresenceXml = ""
    end,
    
    NewUserPresenceListStr = ClientPresenceXml ++ UserPresenceListStr,
    get_sig_client_presence(RosterTab, T, NewUserPresenceListStr).
    


%%get signal roster presence
get_presence(Who, Dict) ->
   
    {_,RosterTab,_} = Dict,
    
    case ets:lookup(RosterTab, Who) of
    
        [{_,TmpTuple}] ->    
            {RosterSatus,RosterShow,RosterPriority} = TmpTuple,        
            if
                is_list(RosterPriority), length(RosterPriority) > 0 ->
                    RosterPriorityStr = RosterPriority;
                true ->
                    RosterPriorityStr = "1"
            end,
        
            ReplyXML = ?XML_HEADER ++ "<presence>" ++ RosterPriorityStr ++ "</presence><status>" ++ RosterSatus ++ "</status><show>" ++ RosterShow ++ "</show>" ++ ?XML_BOTTOM;
        [] ->
            ReplyXML = ?XML_HEADER ++ "<presence>0</presences><status>offline</status><show></show>"
    end,

    {ok, ReplyXML}.


%%sync signal roster presence
%%sync presence into local db, only by robot!!!
sync_presence(_MySession, FromJID, Packet, Dict) ->
    %%io:format("~nFrom:~p~n", [FromJID]),
    %%io:format("~nPacket:~p~n", [Packet]),
    
    %%exmpp_presence:get_show(Packet),
    %%exmpp_presence:get_status(Packet),
    %%exmpp_presence:get_priority(Packet),
    
    From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
    TmpList = string:tokens(From, "@"),
    [FromWho|_T] = TmpList,
    %%io:format("~nFromWho:~p~n", [FromWho]),
    
    
    %%presence of status
    case exmpp_xml:get_element(Packet, status) of
        undefined -> 
            StatusInfo = [];
        Status ->
            StatusInfo = binary_to_list(exmpp_xml:get_cdata(Status))
    end,
    
    %%presence of show
    case exmpp_xml:get_element(Packet, show) of
        undefined -> 
            ShowInfo = [];
        Show -> 
            ShowInfo = binary_to_list(exmpp_xml:get_cdata(Show))
    end,
    
    %%presence of priority
    case exmpp_xml:get_element(Packet, priority) of
        undefined -> 
            PriorityInfo = [];
        Priority ->
            PriorityInfo = binary_to_list(exmpp_xml:get_cdata(Priority))
    end,
    
    io:format("~nFromWho:~p, Status:~p, Show:~p, Priority:~p~n", [FromWho, StatusInfo, ShowInfo, PriorityInfo]),
    
    
    %%sync presence to local db or ets table
    %%set new status into ets table
    {_,RosterTab,_} = Dict,
    ets:insert(RosterTab, {FromWho, {StatusInfo, ShowInfo, PriorityInfo}}),
    
    %%Rec = ets:lookup(RosterTab, FromWho),
    %%io:format("~n-----TTTT----~n"),
    %%io:format("~nRec:~p~n", [Rec]),
    
    ok.
    


%%set presence
%%OnOrOff-> 0:off 1:on
%%Status-> online, chat, away, dnd, xa
set_presence(MySession, OnOrOff, Status) ->

    case OnOrOff of
        1 ->
            PV = exmpp_presence:available();
        2 ->
            PV = exmpp_presence:unavailable();
        _ ->
            PV = exmpp_presence:available()
        end,
    %%set status
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_show(PV, Status)).

    %%exmpp_session:send_packet(MySession,
    %%                          exmpp_presence:set_priority(PV, 2)).
                  
    
