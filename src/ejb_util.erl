%%ejb util functions
%%
-module(ejb_util).
-author("Andy Chow <diudiu8848@gmail.com>").
-vsn(0.1).
-compile(export_all).


%%escape special characters of content
escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C > 16#7f ->
    %% This assumes that characters are at most 16 bits wide.
    escape_byte(((C band 16#c0) bsr 6) + 16#c0)
    ++ escape_byte(C band 16#3f + 16#80)
    ++ escape_uri(Cs);
escape_uri([C | Cs]) -> 
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) -> [].
 
escape_byte(C) ->
    H = hex_octet(C),
    % io:fwrite("~p - ~p~n", [C, H]),
    normalize(H).

%% Append 0 if length == 1
normalize(H) when length(H) == 1 -> "%0" ++ H;
normalize(H) -> "%" ++ H.

%%hex to octet
hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].


%%convert term list to string
%%like: [{x1,y1,z1},{x2,y2,z2}..]
term_list_to_string([H|T], StrList, FatherAppendStr, SonAppendStr) ->
    if 
        is_tuple(H), size(H) > 0 ->
            %%convert tuple to list
            TmpList = tuple_to_list(H),
            
            %%convert signal to string
            TmpStrList = list_to_string(TmpList, [], SonAppendStr),
        
            if
                is_list(StrList), length(StrList) > 0 ->
                    NewStrList = StrList ++ FatherAppendStr ++ TmpStrList;
                true ->
                    NewStrList = TmpStrList
            end;
        true ->
            if
                is_list(StrList), length(StrList) > 0 ->
                    NewStrList = StrList ++ FatherAppendStr ++ H;
                true ->
                    NewStrList = H
            end
    end,
    
    %%do for next element
    term_list_to_string(T, NewStrList, FatherAppendStr, SonAppendStr);
    
term_list_to_string([], StrList, _FatherAppendStr, _SonAppendStr) ->
    StrList.


%%convert signal tuple as string
%%StrList-> tmp formated string list
%%AppendStr -> like ',' or '|'
list_to_string([H|T], StrList, AppendStr) ->    
    if
        is_integer(H) ->
            SigStr = int_to_string(H);
        is_atom(H) ->
            SigStr = atom_to_list(H);
        true ->
            SigStr = H
    end,
    
    if
        is_list(StrList), length(StrList) > 0 ->
            NewStrList = StrList ++ AppendStr ++ SigStr;
        true ->
            NewStrList = SigStr
    end,
    
    list_to_string(T, NewStrList, AppendStr);
list_to_string([], StrList, _AppendStr) ->
    StrList.



%%convert string into integer, and rem it
%%str_rem("test", 9) -> 0~8
str_rem(Str, MaxVal) ->
    Bin = erlang:md5(Str),
    L = binary_to_list(Bin),
    {_, Total} = lists:mapfoldl(fun(X, Sum) -> {X, X+Sum} end, 0, L),
    RemVal = Total rem MaxVal,
    {ok, RemVal}.


%%ceil float values
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.


%%floor float values
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


%%for, loop
for(Max,Max,F) -> [F(Max)]; 
for(I,Max,F) -> [F(I)|for(I+1,Max,F)].


%%integer to string
int_to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).

%%list to term
list_to_term([_H|T]) ->
    %%io:format("~nMMMM:~p~n", [H]),
    list_to_term(T);
list_to_term([]) ->
    "".
    

