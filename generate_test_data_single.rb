#!/usr/bin/env ruby
# generate test data with a single median element slap bang in the middle

def generate min, median, max, number, seed=nil
	raise "need min < median < max" unless min < median and median < max
	srand(seed) if seed

	info = File.new("numbers.info","w")
	info.puts "min=#{min} median=#{median} max=#{max} number=#{number} seed=#{seed}"
	info.close

	num_less_than = number / 2
	num_greater_than = number / 2

	less_than_range = median - min
	greater_than_range = max - median

	total = num_less_than + num_greater_than + 1
	median_idx = ( num_less_than + num_greater_than ) / 2

	idx = 0
	while(total > 0) do
		if (idx == median_idx)
			puts median
		else
			case rand(2)
				when 0 
					if num_less_than > 0
						puts rand(less_than_range) + min 
						num_less_than -= 1							
					end
				when 1
					if num_greater_than > 0
						puts rand(greater_than_range) + median + 1
						num_greater_than -= 1
					end
			end
		end
	
		total = num_less_than + num_greater_than
		idx += 1
	end

end

raise "generate_test_data.rb min median max number_elements (seed)" unless ARGV.length==5 || ARGV.length==4
ARGV.map! { |arg| arg.to_f.to_i } # to_f to allow 1e6 notation
min, median, max, number,  seed = ARGV
generate min, median, max, number, seed

