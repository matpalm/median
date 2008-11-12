-module(worker).
-export([init/1]).
%-compile(export_all).

init(File) ->
    List = parse_file:to_list(File),
    loop(List).

loop(List) ->
%    io:format("~w entering loop with ~w\n",[self(),List]),
    receive
	{ request, length } ->
	    controller ! { length, length(List) },
	    loop(List);

	{request, have_data} ->
	    case length(List) == 0 of
		true ->
		    controller ! dead,
		    no_more_data;
		false ->
		    controller ! { alive, self() },
		    loop(List)
	    end;

	{ request, min_max } ->
	    controller ! { min_max, { lists:min(List), lists:max(List) } },
	    loop(List);

	{ request, pivot } ->
	    controller ! { pivot, hd(List) },
	    loop(List);

	{ request, {less_than,N} = LtMsg } ->
	    Num_less_than = number_less_than(List, N),
	    controller ! { LtMsg, Num_less_than },
	    loop(List);

	rotate ->
	    loop(rotate(List));

	{ filter_gt_eq, N } ->
	    loop(lists:filter(fun(E) -> E >= N end, List));

	{ filter_lt, N } ->
	    loop(lists:filter(fun(E) -> E < N end, List));

	shutdown ->
	    shutdown;

	Msg ->
	    io:format("~w received unexpected Msg ~w\n",[self(),Msg]),
	    fail_dog

    
    after 1500 ->
	    io:format("~w timeout\n",[self()])

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
