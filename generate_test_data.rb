#!/usr/bin/env ruby

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

	# decide how many copies of median to emit
	# make it the average of the expected frequencies less than and greater than
	# need to ensure it's a multiple of three so it retains it's median status
	expected_freq_values_less_than = num_less_than.to_f / less_than_range
	expected_freq_values_greater_than = num_greater_than.to_f / greater_than_range
	average_freq = ((expected_freq_values_less_than + expected_freq_values_greater_than )/2).to_i
	total_eq = average_freq/3*3 
	total_eq = 1 if total_eq == 0

	total = num_less_than + num_greater_than + total_eq

	while(total > 0) do
		case rand(3)
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
			when 2
				if total_eq > 0
					puts median
					total_eq -= 1
				end
		end
	
		total = num_less_than + num_greater_than + total_eq

	end

end

raise "generate_test_data.rb min median max number_elements (seed)" unless ARGV.length==5 || ARGV.length==4
ARGV.map! { |arg| arg.to_f.to_i } # to_f to allow 1e6 notation
min, median, max, number,  seed = ARGV
require 'generate_test_data.rb'
generate min, median, max, number, seed

