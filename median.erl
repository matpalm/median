-module(median).
-export([from_file/1,from_list/1]).
%-compile(export_all).

from_file(File) ->
    io:format("~w\n",[from_list(parse_file:to_list(File))]).

from_list(List) ->
    nth_order_stat(round(length(List)/2), List).

nth_order_stat(Target_order_stat, List) ->
    Min = lists:min(List),
    Max = lists:max(List),
    if 
	(Min == Max) or (Target_order_stat == 1) ->
	    Min;
	Target_order_stat == length(List) ->
	    Max;
	true ->
	    partition_on_pivot(Target_order_stat, List)	
    end.

partition_on_pivot(Target_order_stat, List) ->
    Pivot = hd(List),
    Lt_pivot_predicate = fun(X) -> X < Pivot end,
    Num_less_than = count_times_predicate_true(Lt_pivot_predicate, List),
    Pivot_order_stat = Num_less_than + 1,

%    io:format("Target_order_stat=~w List=~w Pivot=~w Pivot_order_stat=~w\n",
%	      [Target_order_stat,List,Pivot,Pivot_order_stat]),

    if 
	Pivot_order_stat == Target_order_stat ->
	    Pivot;

	Pivot_order_stat == 1 ->
	    nth_order_stat(Target_order_stat, rotate(List));

	Pivot_order_stat < Target_order_stat ->
	    Lt = lists:filter(fun(E) -> E >= Pivot end, List),
	    Rotated = rotate(Lt),
	    Adjusted_target_order_stat = Target_order_stat - Num_less_than, 
	    nth_order_stat(Adjusted_target_order_stat, Rotated);

	true -> % Pivot_order_stat > Target_order_stat
	    Gt = lists:filter(Lt_pivot_predicate, List),
	    nth_order_stat(Target_order_stat, Gt)
    end.   

count_times_predicate_true(Predicate, List) ->
    lists:foldl(fun(X,Acc) ->
			case Predicate(X) of
			    true ->
				Acc+1;
			    false ->
				Acc
			end
		end,
		0, List).

rotate([H|T]) -> 
    T ++ [H].


    




	    
    

			 
			 
			  



