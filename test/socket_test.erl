-module(socket_test).
-compile(export_all).


test(Begin, End) ->
    List = build_list(Begin, End),
    send(List).
    
    
    
send([H|T]) ->
    IP = "127.0.0.1",
    Port = 5555,
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
    Msg = "test" ++ int_to_string(H),
    Msg2 = "login," ++ Msg ++ "," ++ Msg,
    io:format("~n~p~n", [Msg2]),
    ok = gen_tcp:send(Socket, Msg2),
    send(T);        
send([]) ->
    void.


build_list(Min, Max) ->
    F = fun(X) -> X end,
    for(Min, Max, F).


%%for, loop
for(Max,Max,F) -> [F(Max)]; 
for(I,Max,F) -> [F(I)|for(I+1,Max,F)].

%%integer to string
int_to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).
    
    