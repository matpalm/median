-module(generate_binary_dicts).
-export([main/1,writer/1]).

main([InFile,OutFilePrefix,OutFileNum]) ->
    register(controller, self()),
    io:format("InFile ~p OutFilePrefix ~p OutFileNum ~p\n",[InFile,OutFilePrefix,OutFileNum]),
    Writers = spawn_writers(OutFilePrefix,list_to_integer(OutFileNum)),
    send_file_data_to_writers(Writers,InFile),
    send_prepare_for_flush(Writers),
    flush_writers_sequentially(Writers),
    init:stop().

spawn_writers(Output,Num) ->
    spawn_writers(Output,Num,[]).

spawn_writers(_O,0,Writers) -> 
    Writers;

spawn_writers(Output,Num,Writers) ->
    OutputFile = Output ++ "." ++ integer_to_list(Num-1),
    Writer = spawn(?MODULE,writer,[OutputFile]),
    spawn_writers(Output,Num-1,[Writer|Writers]).


send_file_data_to_writers(Writers,InFile) ->
    {ok,File} = file:open(InFile,read),
    send_line_to_writer(Writers,Writers,File,next_line(File)).

send_line_to_writer(_,_,File,eof) ->
    file:close(File),
    done;

send_line_to_writer([Writer|T],Writers,File,Line) ->
    Writer ! { line, Line },
    send_line_to_writer(T,Writers,File,next_line(File));

send_line_to_writer([],Writers,File,Line) ->
    send_line_to_writer(Writers,Writers,File,Line).


send_prepare_for_flush(Writers) ->
    lists:foreach(
      fun(W) -> W ! prepare_for_flush end,
      Writers
     ).	     


flush_writers_sequentially([]) ->
    done;

flush_writers_sequentially([Writer|T]) ->
    Writer ! flush,
    receive ack -> done end,
    flush_writers_sequentially(T).


next_line(File) ->	
    Line = io:get_line(File,''),
    case Line of
	eof -> eof;
	_ -> lists:sublist(Line, length(Line)-1)
    end.

    
writer(Filename) ->
    writer(Filename,dict:new()).

writer(Filename, Dict) ->
    receive

	{line,Line} ->
	    Int = list_to_integer(Line),
	    NewDict = dict:update_counter(Int,1,Dict),
	    writer(Filename, NewDict);
	
	prepare_for_flush ->
	    io:format("~w preparing for flush\n",[self()]),
	    writer_flush(Filename,term_to_binary(Dict));

	Msg ->
	    io:format("~w got unexpected ~w\n",[self(),Msg])

    after 1000 ->
	    io:format("~w timeout\n",[self()])
    end.
    
writer_flush(Filename,Binary) ->
    receive
	flush ->
	    io:format("~w flushing ~p\n",[self(),Filename]),
	    {ok,F} = file:open(Filename,write),
	    file:write(F,Binary),
	    file:close(F),
	    controller ! ack
    end.
