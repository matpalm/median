-module(parse_file).
-export([parse/1, parse_to_dict/1]).

parse(Filename) ->
    {ok,File} = file:open(Filename,read),
    parse(File, [], next_line(File)).

parse(File,Numbers,eof) -> 
    file:close(File),
    lists:reverse(Numbers);

parse(File,Numbers,Line) ->
    Value = line_to_int(Line),
    parse(File, [Value|Numbers], next_line(File)).


parse_to_dict(Filename) ->
    {ok,File} = file:open(Filename,read),
    parse_to_dict(File, dict:new(), next_line(File)).

parse_to_dict(File,Freqs,eof) ->
    file:close(File),
    Freqs;

parse_to_dict(File,Freqs,Line) ->
    Value = line_to_int(Line),
    parse_to_dict(File, dict:update_counter(Value,1,Freqs), next_line(File)).

    
line_to_int(Line) ->
    Line_without_cr = lists:sublist(Line, length(Line)-1),
    list_to_integer(Line_without_cr).

next_line(File) ->
    io:get_line(File,'').
    
