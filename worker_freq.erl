-module(worker_freq).
-export([init/1]).
%-compile(export_all).

init(File) ->
    Freqs = parse_file:to_dict_from_binary(File),
    loop({Freqs, dict:fetch_keys(Freqs)}).

loop({Freqs, Keys}=State) ->
%    io:format("~w entering loop with freqs:~w keys:~w\n",[self(),dict:to_list(Freqs),Keys]),
    receive
	{ request, length } ->
	    controller ! { length, sum_values(Freqs) },
	    loop(State);

	{request, have_data} ->
	    case dict:size(Freqs) == 0 of
		true ->
		    controller ! dead,
		    no_more_data;
		false ->
		    controller ! { alive, self() },
		    loop(State)
	    end;

	{ request, min_max } ->
	    controller ! { min_max, { lists:min(Keys), lists:max(Keys) } },
	    loop(State);

	{ request, pivot } ->
	    controller ! { pivot, hd(Keys) },
	    loop(State);

	{ request, {less_than,N} = LtMsg } ->
	    Num_less_than = number_less_than(Freqs, N),
	    controller ! { LtMsg, Num_less_than },
	    loop(State);

	rotate ->
	    loop({Freqs,rotate(Keys)});

	{ filter_gt_eq, N } ->
	    Filtered = dict:filter(
			 fun(Value,_) -> Value >= N end,
			 Freqs
			),
	    loop({Filtered, dict:fetch_keys(Filtered)});

	{ filter_lt, N } ->
	    Filtered = dict:filter(
			 fun(Value,_) -> Value < N end,
			 Freqs
			),
	    loop({Filtered, dict:fetch_keys(Filtered)});	
		      
	shutdown ->
	    shutdown;
	
	Msg ->
	    io:format("~w received unexpected Msg ~w\n",[self(),Msg]),
	    fail_dog

    
    after 1500 ->
	    io:format("~w timeout\n",[self()])

    end.

sum_values(Freqs) ->
    dict:fold(
      fun(_,Freq,Acc) -> Acc + Freq end, 
      0, Freqs
     ).
	    
rotate([H|T]) -> 
    T ++ [H].

number_less_than(Freqs, N) -> 
    dict:fold(
      fun(Num,Freq,Acc) ->
	      case Num < N  of
		  true ->
		      Acc + Freq;
		  false ->
		      Acc
	      end
      end,
      0, Freqs).    
