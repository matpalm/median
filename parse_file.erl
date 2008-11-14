-module(parse_file).
-export([to_list/1, to_dict/1, to_dict_from_binary/1, to_dict_b/1, test/1]).

to_list(Filename) ->
    {ok,File} = file:open(Filename,read),
    to_list(File, [], next_line(File)).

to_list(File,Numbers,eof) -> 
    file:close(File),
    lists:reverse(Numbers);

to_list(File,Numbers,Line) ->
    Value = line_to_int(Line),
    to_list(File, [Value|Numbers], next_line(File)).


to_dict_from_binary(Filename) ->
    {ok,B} = file:read_file(Filename),
    binary_to_term(B).
        
    
to_dict(Filename) ->
    {ok,File} = file:open(Filename,read),
    to_dict(File, dict:new(), next_line(File)).

to_dict(File,Freqs,eof) ->
    file:close(File),
    Freqs;

to_dict(File,Freqs,Line) ->
    Value = line_to_int(Line),
    to_dict(File, dict:update_counter(Value,1,Freqs), next_line(File)).

next_line(File) ->
    io:get_line(File,'').
    
line_to_int(Line) ->
    Line_without_cr = lists:sublist(Line, length(Line)-1),
    list_to_integer(Line_without_cr).

to_dict_b(InFilename) ->
    {ok,B} = file:read_file(InFilename),
    to_dict_b(InFilename,B,dict:new()).

to_dict_b(_InFilename, <<>>, Dict) ->
    Dict;
    
to_dict_b(InFilename, Binary, Dict) ->
    { ReducedBinary, Line } = next_line_b(Binary),
    Int = list_to_integer(Line),
    NewDict = dict:update_counter(Int,1,Dict),
    to_dict_b(InFilename, ReducedBinary, NewDict).

next_line_b(Binary) ->
    next_line_b(Binary, []).

next_line_b(<<>>, _Collected) ->
    ignore_last_line_if_didnt_end_in_newline;

next_line_b(<<"\n",Rest/binary>>, Collected) ->  
    { Rest, binary_to_list(list_to_binary(lists:reverse(Collected))) }; % black magic voodoo line

next_line_b(<<C:1/binary,Rest/binary>>, Collected) ->
    next_line_b(Rest, [C|Collected]). 



% time testing

test([Call,File]) ->
    Start = now(),
    apply(?MODULE, list_to_atom(Call), [File]),
    Seconds = timer:now_diff(now(),Start) / 1000 / 1000,
    io:format("~s time ~w sec\n",[Call, Seconds]),
    init:stop().

