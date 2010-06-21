%%ejb socket server
%%
-module(ejb_socket_server).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).
-behaviour(gen_server).

-include("ejb.hrl").
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).

%%API
-export([start/1, stop/0]).

%%CallBack
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


%%%%%%%%%%%%%%%%%%%%

%%start socket server
start(Args) ->
    %%get sock cfg 
    {_Local, SockServCfg} = Args,
    
    %%start gen server
    case gen_server:start({local, ?MODULE}, ?MODULE, SockServCfg, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Other ->
            Other
    end.


%%server stop
stop() ->
    gen_server:call(?MODULE, stop).


%%server init
init(Args) ->
    process_flag(trap_exit, true),
    
    %%{ok, DbServCfg} = application:get_env(?MODULE, db_setup),
    State = "ok",    
    [SockPort|_T] = Args,
    
    if 
        is_integer(SockPort), SockPort > 0 ->
            SockServPort = SockPort;
        true->
            SockServPort = ?AJABBERD_SERVER_PORT
    end,
    
    %%init socket listen
    Pid = spawn(fun() -> manage_clients([]) end),
    register(client_manager, Pid),
    
    %%listen port
    {ok, Listener} = gen_tcp:listen(SockServPort, ?TCP_OPTIONS),
   
    %%spawn new process
    SFun = fun() -> process_connect(Listener) end,
    spawn(SFun),
    
    {ok, State}.

%%process signal client connect
process_connect(Listener) ->
    %%accept signal connect
    case gen_tcp:accept(Listener) of
    
        {ok, Socket} ->
            spawn(fun() -> process_connect(Listener) end),
    
            %%send socket handle to manager
            client_manager ! {connect, Socket},
    
            %%wait connected client command
            wait_client(Socket);
        
        {error,closed} ->
            void
    end.
    

%%wait and process connected client command
wait_client(Socket) ->
    receive
        {tcp, Socket, Data} ->
            process_client_command(Socket, Data),
            wait_client(Socket);
        {tcp_closed, Socket} ->
            client_manager ! {disconnect, Socket};
        {error,closed} ->
            client_manager ! {disconnect, Socket};            
        _Any ->
            wait_client(Socket)
    end.


%%client command process
process_client_command(Socket, Data) ->    
    if 
        is_binary(Data), size(Data) > ?AJABBERD_SOCK_DATA_MIN ->
            %%call ejb_service_manage module
            {_Status, Reply} = ejb_service_manager:process_command(Socket, Data);
        true ->
            Reply = ?XML_HEADER ++ "<ret>-1</ret><reason>Invalid Data</reason>" ++ ?XML_BOTTOM
    end,
    
    %%reformat reply data for client
    %%io:format("~nRStatus:~p~n", [RStatus]),    
    ClientReply = Reply,
    
    %%send data to client
    gen_tcp:send(Socket, ClientReply).
    

%%client manager
manage_clients(Sockets) ->
    receive
        {connect, Socket} ->
            %%increate socket list
            NewSockets = [Socket|Sockets],
            _Total_clients = length(Sockets) + 1;
        {disconnect, Socket} ->
            %%decreate socket list
            NewSockets = lists:delete(Socket, Sockets),            
            _Total_clients = length(Sockets) - 1;
        _Any ->
            NewSockets = Sockets,
            _Total_clients = length(Sockets)
    end,
    %%io:format("~ntotal clients:~p~n", [Total_clients]),
    manage_clients(NewSockets).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%private functions%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%call backs
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

