-module(controller).
-export([init/1]).
%-compile(export_all).

init([Worker_impl|Files]) ->
    put(workers, spawn_workers(net_adm:world(),list_to_atom(Worker_impl),Files)),
    Number_elements = lists:sum(workers_request(length)),
    M = median(Number_elements),
    io:format("~w\n",[M]),
    init:stop().

spawn_workers(Nodes, Worker_Impl, Files) ->
    spawn_workers(Nodes, Worker_Impl, Files, []).

spawn_workers([], Worker_impl, Files, Workers) ->
    spawn_workers(net_adm:world(), Worker_impl, Files, Workers);

spawn_workers(_Nodes, _Worker_impl, [], Workers) ->
    Workers;

spawn_workers([Node|OtherNodes],Worker_impl,[File|OtherFiles], Workers) ->
    ControllerPid = self(),
    io:format("spawn worker for ~p on ~p\n",[File,Node]),
    New_worker = spawn(Node, Worker_impl, init, [ControllerPid,File]),
    spawn_workers(OtherNodes, Worker_impl, OtherFiles, [New_worker|Workers]).

median(Number_elements) ->
    nth_order_stat(round(Number_elements/2)).

nth_order_stat(Target_order_stat) ->
    prune_workers_without_data(),
    { Min, Max } = workers_min_max(),
    % TODO do we ever exit on Max clause?
    if 
	(Min == Max) or (Target_order_stat == 1) ->
	    Min;    
	true ->
	    partition_on_pivot(Target_order_stat)	
    end.
    
partition_on_pivot(Target_order_stat) ->
    Pivot = select_pivot(),
    Num_less_than = lists:sum(workers_request({less_than,Pivot})),
    Pivot_order_stat = Num_less_than + 1, 

    if 
	Pivot_order_stat == Target_order_stat ->
	    broadcast_to_workers(shutdown),
	    Pivot;

	Pivot_order_stat == 1 ->
	    rotate(),
	    nth_order_stat(Target_order_stat);

	Pivot_order_stat < Target_order_stat ->
	    broadcast_to_workers({filter_gt_eq, Pivot}),
	    rotate(),
	    Adjusted_target_order_stat = Target_order_stat - Num_less_than, 
	    nth_order_stat(Adjusted_target_order_stat);

	true -> % Pivot_order_stat > Target_order_stat
	    broadcast_to_workers({filter_lt, Pivot}),
	    nth_order_stat(Target_order_stat)
    end.

select_pivot() ->
    hd(get(workers)) ! { request, pivot},
    receive
	{ pivot, Pivot} ->
	    Pivot
    end.   

rotate() -> 
    [H|T] = get(workers),
    H ! rotate,
    put(workers, T++[H]).

prune_workers_without_data() ->
    broadcast_to_workers({request, have_data}),
    Alive_workers = receive_alive_workers(length(get(workers)), []),
    put(workers, Alive_workers).

receive_alive_workers(0, Alive_workers) ->
    lists:reverse(Alive_workers);

receive_alive_workers(Num_remaining, Workers) ->
    receive
	{ alive, WorkerPid } ->	    
	    receive_alive_workers(Num_remaining-1, [WorkerPid|Workers]);
	dead ->
	    receive_alive_workers(Num_remaining-1, Workers)
    end.

workers_min_max() ->
    MinMaxs = workers_request(min_max), % list of min/maxs [ {2,3}, {0,4}, {4,9} ... ]
    lists:foldl(
      fun({NextMin,NextMax},{Min,Max}) -> { min(NextMin,Min), max(NextMax,Max) } end,
      hd(MinMaxs),
      MinMaxs
     ).
	
min(A,B) ->
    case A < B of
	true -> A;
	false -> B
    end.

max(A,B) ->
    case A > B of
	true -> A;
	false -> B
    end.    
	    
workers_request(Type) ->
    % request 
    [ P ! { request, Type } || P <- get(workers) ],
    % receive responses
    [ receive
	  { _Type, N } ->
	      N
      after 60000 ->
	      io:format("missed a response\n")
      end 
      || _P <- get(workers)
    ].

broadcast_to_workers(Msg) ->
    [ P ! Msg || P <- get(workers)].
    

			 
			 
			  



