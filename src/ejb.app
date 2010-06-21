{application, ejb,
 [
  {description, "EJB, jabber service"},
  {vsn, "0.1"},
  {modules, [ejb, ejb_sup_server, ejb_serv]},
  {registered, [ejb, ejb_sup_server, ejb_serv]},
  {applications, [kernel, stdlib]},
  {mod, {ejb, []}},
  {env, [
	 %%jabber server setup
   	 {jb_setup, ["127.0.0.1", 5222]},

	 %%jabber local db setup, for robot mainly.
	 {robot_setup, [{"127.0.0.1", "test", "test", "test", 2}, {"robot", "robot"}]},

	 %%ejb socket service
	 {sock_setup, [5555]}
	 ]}
 ]
 }.
