def median list
	target_order_stat = (list.size.to_f / 2 ).ceil
	while true do
		min,max = list.min, list.max
		return min if min == max or target_order_stat == 1
		return max if target_order_stat == list.size
	  pivot = list.first
	  num_less_than = list.inject(0) { |n,e| e < pivot ? n+1 : n }
    pivot_order_stat = num_less_than + 1
    if pivot_order_stat == target_order_stat
      return pivot
		elsif pivot_order_stat == 1
			list << list.shift
    elsif pivot_order_stat < target_order_stat
      list.reject! { |e| e < pivot }
			target_order_stat -= num_less_than
			list << list.shift
    else # => pivot_order_stat > target_order_stat
	    list.reject! { |e| e >= pivot }
    end
  end
end
