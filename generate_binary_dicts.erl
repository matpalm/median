-module(generate_binary_dicts).
-export([main/1,writer/1]).

main(Files) ->    
    register(controller, self()),
    Writers = spawn_writers(Files),
    wait_for_writers(Writers),
    init:stop().


spawn_writers(Files) ->
    spawn_writers(Files,[]).

spawn_writers([],Writers) -> 
    Writers;

spawn_writers([File|OtherFiles],Writers) ->
    Writer = spawn(?MODULE,writer,[File]), % change this back to worker for io next_line version
    spawn_writers(OtherFiles,[Writer|Writers]).


wait_for_writers([]) ->
    done;

wait_for_writers([H|T]) ->
    H ! ping,
    receive ack -> ok end,
    wait_for_writers(T).
   

writer(InFilename) ->
    Dict = parse_file:to_dict_b(InFilename),
    OutputFilename = InFilename ++ ".dict",
    {ok,F} = file:open(OutputFilename,write),
    file:write(F,term_to_binary(Dict)),
    file:close(F),
    receive ping -> controller ! ack end.

