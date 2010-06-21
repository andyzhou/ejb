%%ejb roster module
%%
-module(ejb_mod_roster).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").

-export([get_roster_cmd/1, store_roster_info/3]).
-export([get_roster_list/2]).


%%get roster list of signal client
get_roster_list(User, Dict) ->
    Ets_roster_key = User ++ "_rosters",
    
    {_,RosterTab,_} = Dict,
    Rec = ets:lookup(RosterTab, Ets_roster_key),
    
    [H|_T] = Rec,
    {_,RosterList} = H,
    %%io:format("~nRosterList:~p~n", [RosterList]),
    
   
    %%convert roster list into xml stream
    RosterXMLStr = roster_list_to_xml(RosterList, [], Dict),
    RosterListXML = ?XML_HEADER ++ "<rosters>" ++ RosterXMLStr ++ "</rosters>" ++ ?XML_BOTTOM,
    
    %%io:format("~nRosterListStr:~p~n", [RosterListXML]),
    
    {ok, RosterListXML}.


%%send get roster info command to im server
get_roster_cmd(MySession) ->
    Query = #xmlel{ns = ?NS_ROSTER, name = 'query'},
    Iq = exmpp_xml:set_attributes(
	   #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
	   [{'type', "get"}]),
    Packet = exmpp_xml:append_child(Iq, Query),
    _PacketId = exmpp_session:send_packet(MySession, Packet),
    ok.


%%storage assigned client roster info into local ets table
%%roster list like: [{user,status,presence},...]
store_roster_info(User, RosterPacket, Dict) ->

    %%io:format("~nPacket:~p~n", [RosterPacket]),
    {_, _, _, _, _, RosterInfo} = RosterPacket,
    [{_, _, _, _, _, RosterList}] = RosterInfo,
    
    %%io:format("~nL:~p~n", [RosterList]),
    
    %%analize roster list, and rebuild new roster list
    NewRosterList =  analize_roster_list(RosterList, []),
    
    %%io:format("~n------:~p~n", [NewRosterList]),
    
    %%storage new roster list into local ets table
    Ets_roster_key = User ++ "_rosters",
    
    {_,RosterTab,_} = Dict,
    ets:insert(RosterTab, {Ets_roster_key, NewRosterList}),
    
    %%VV = ets:lookup(RosterTab, Ets_roster_key),
    %%io:format("~nvvv:~p~n", [VV]),
    
    ok.



%%analize roster list
analize_roster_list([H|T], RosterLists) ->
    {_, _, _, _, SigRoster,_} = H,
    
    if 
        is_list(SigRoster), length(SigRoster) >= 2 ->
    
            %%conver list to tuple
            LL = list_to_tuple(SigRoster),
        
            %%get roster status and name
            Status = binary_to_list(element(4, element(1,LL))),
            Jid = binary_to_list(element(4, element(2, LL))),
            Presence = "offline",
        
            if
                Jid =/= "none" ->  
                    %%reformat new list, set presence default offline
                    TmpSigRec = {Jid, Status, Presence},
                    NewRosterLists = [TmpSigRec|RosterLists],                
                    analize_roster_list(T, NewRosterLists);
                true ->
                    analize_roster_list(T, RosterLists)
            end;
        
        true ->
            analize_roster_list(T, RosterLists)
    end;
analize_roster_list([], RosterLists) ->
    RosterLists.



%%%
%%convert rosters list to xml format stream
%%list like: [{x1,y1,z1},{x2,y2,z2}..]
roster_list_to_xml([H|T], StrList, Dict) ->

    if 
        is_tuple(H), size(H) > 0 ->
            %%convert tuple to list
            TmpList = tuple_to_list(H),
            
            %%convert signal roster to xml stream
            case sig_roster_to_xml(TmpList, Dict) of
                {ok, SigRosterXml} ->        
                    if 
                        is_list(StrList), length(StrList) > 0 ->
                            NewStrList = StrList ++ SigRosterXml;
                        true ->
                            NewStrList = SigRosterXml
                    end;
                _ ->
                    NewStrList = StrList
            end;
        true ->
            if
                is_list(StrList), length(StrList) > 0 ->
                    NewStrList = StrList ++ "<roster>" ++ H ++ "</roster>";
                true ->
                    NewStrList = "<roster>" ++ H ++ "</roster>"
            end
    end,
    
    %%do for next element
    roster_list_to_xml(T, NewStrList, Dict);
    
roster_list_to_xml([], StrList, _Dict) ->
    StrList.


%%convert signal roster tuple as xml stream
%%StrList-> tmp formated string list
sig_roster_to_xml(SigRosterList, Dict) ->
    if
        is_list(SigRosterList), length(SigRosterList) > 1 ->
            
            %%analize signal roster info
            [Roster,_Status,_Presence] = SigRosterList,
        
            %%get current roster status, presence info from rosters ets table
            TmpList = string:tokens(Roster, "@"),
            [RosterName|_] = TmpList,
            {_,RosterTab,_} = Dict,
            Rec = ets:lookup(RosterTab, RosterName),
        
            case Rec of
                [{_,TmpTuple}] ->
                    %%Status:"hi", Show:"dnd", Priority:"6"
                    {RosterSatus,RosterShow,RosterPriority} = TmpTuple;
                _->
                    RosterSatus = "",
                    RosterShow = "",
                    RosterPriority = ""
            end,
            %%io:format("~nRRRRRec:~p~n", [Rec]),
        
            XMLStr = "<roster><who>" ++ RosterName ++ "</who><status><![CDATA[" ++ RosterSatus ++ "]]></status><show>" ++ RosterShow ++ "</show><priority>" ++ RosterPriority ++ "</priority></roster>",
            {ok, XMLStr};
        true ->
            {error, "No Record"}
    end.
