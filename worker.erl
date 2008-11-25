-module(worker).
-export([init/2]).
%-compile(export_all).

init(Controller,File) ->
    put(controller, Controller),
    List = parse_file:to_list(File),
    loop(List).

loop(List) ->
    Controller = get(controller),
    receive
	print_debug_info ->
	    io:format("~w #elems=~w min=~w max=~w elems=~w \n",[self(),length(List),lists:min(List),lists:max(List),List]),
	    loop(List);

	{ request, length } ->
	    Controller ! { length, length(List) },
	    loop(List);

	{request, have_data} ->
	    case length(List) == 0 of
		true ->
		    Controller ! dead,
		    no_more_data;
		false ->
		    Controller ! { alive, self() },
		    loop(List)
	    end;

	{ request, min_max } ->
	    Controller ! { min_max, { lists:min(List), lists:max(List) } },
	    loop(List);

	{ request, pivot } ->
	    Controller ! { pivot, hd(List) },
	    loop(List);

	{ request, {less_than,N} = LtMsg } ->
	    Num_less_than = number_less_than(List, N),
	    Controller ! { LtMsg, Num_less_than },
	    loop(List);

	rotate ->
	    loop(rotate(List));

	{ filter_gt_eq, N } ->
	    loop([X || X <- List, X >= N]);

	{ filter_lt, N } ->
	    loop([X || X <- List, X < N]);

	shutdown ->
	    shutdown;

	Msg ->
	    io:format("~w received unexpected Msg ~w\n",[self(),Msg]),
	    fail_dog

    end.

	    
rotate([H|T]) -> 
    T ++ [H].

number_less_than(List,N) -> 
    lists:foldl(fun(X,Acc) ->
			case X < N  of
			    true ->
				Acc+1;
			    false ->
				Acc
			end
		end,
		0, List).    
