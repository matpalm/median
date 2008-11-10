-module(parse_file).
-export([parse/1]).

parse(Filename) ->
    {ok,File} = file:open(Filename,read),
    parse(File, [], io:get_line(File,'')).

parse(File,Numbers,eof) -> 
    file:close(File),
    lists:reverse(Numbers);

parse(File,Numbers,Line) ->
    Line_without_cr = lists:sublist(Line, length(Line)-1),
    As_int = list_to_integer(Line_without_cr),
    parse(File, [As_int|Numbers], io:get_line(File,'')).
