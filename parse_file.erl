-module(parse_file).
-export([to_list/1, to_dict/1]).

to_list(Filename) ->
    {ok,File} = file:open(Filename,read),
    to_list(File, [], next_line(File)).

to_list(File,Numbers,eof) -> 
    file:close(File),
    lists:reverse(Numbers);

to_list(File,Numbers,Line) ->
    Value = line_to_int(Line),
    to_list(File, [Value|Numbers], next_line(File)).


to_dict(Filename) ->
    {ok,File} = file:open(Filename,read),
    to_dict(File, dict:new(), next_line(File)).

to_dict(File,Freqs,eof) ->
    file:close(File),
    Freqs;

to_dict(File,Freqs,Line) ->
    Value = line_to_int(Line),
    to_dict(File, dict:update_counter(Value,1,Freqs), next_line(File)).

    
line_to_int(Line) ->
    Line_without_cr = lists:sublist(Line, length(Line)-1),
    list_to_integer(Line_without_cr).

next_line(File) ->
    io:get_line(File,'').
    
