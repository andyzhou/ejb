-include_lib("xmerl/include/xmerl.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("../3rd/mysql-driver/src/mysql.hrl").


-define(XML_HEADER, "<?xml version=\"1.0\" encoding=\"utf-8\"?><xml>").
-define(XML_BOTTOM, "</xml>\n\r").

-define(AJABBERD_ROBOT_NAME, "robot").

-define(AJABBERD_SOCK_DATA_MIN, 10).
-define(AJABBERD_USER_MIN_LEN, 3).
-define(AJABBERD_USER_MAX_LEN, 20).
-define(AJABBERD_PASSWD_MIN_LEN, 3).
-define(AJABBERD_PASSWD_MAX_LEN, 20).

%%edb db server
-define(EDB_SERV_ADDR, "127.0.0.1").
-define(EDB_SERV_PORT, 5554).

-define(AJABBERD_SERVER, "localhost").
-define(AJABBERD_SERVER_PORT, 5555).

-define(EJABBERD_DOMAIN, "im.izhuozhuo.com").
-define(EJABBERD_PORT, 5222).

-define(EJB_MOD_SERVER, ?MODULE).