%%ejb subscribe module
%%
-module(ejb_mod_subscribe).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).

-include("ejb.hrl").

%%API
-export([subscribe_process/2, subscribed_process/5]).
-export([subscribe_roster/2]).


%%subscribe new roster
subscribe_roster(MySession, Who) ->
    ToJID = Who ++ "@" ++ ?EJABBERD_DOMAIN,
    Subscribe = exmpp_presence:subscribe(),
    SubscribeExt = exmpp_xml:set_attribute(Subscribe, to, ToJID),
    exmpp_session:send_packet(MySession, SubscribeExt).
    
%%subscribe process
subscribe_process(MySession, FromJID) ->
    From = exmpp_jid:to_list(exmpp_jid:make(FromJID)),
    %%io:format("~nsubscribe from:~p~n", [From]),
    Subscribed = exmpp_presence:subscribed(),
    SubscribedExt = exmpp_xml:set_attribute(Subscribed, to, From),
    exmpp_session:send_packet(MySession, SubscribedExt).


%%subscribed process
subscribed_process(_MySession, _User, _FromJID, _Packet, _Dict) ->
    %%get old roster list from ets table <
    ok.
