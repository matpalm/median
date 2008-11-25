#!/usr/bin/env ruby
# generate scripts to generate test data on a number of machines
# ./ec2_scripts/generate_generate_scripts.rb 1 314159265 1e9 3e9 3 8
# generates 3 sh scripts, each to generate 8 files to generate a total of 3e9 elements across all 24 files

raise "generate_generate_scripts.rb min median max num_elements num_servers procs_per_server" unless ARGV.length==6 
ARGV.map! { |arg| arg.to_f.to_i } # to_f to allow 1e6 notation
min, median, max, num_elements, num_servers, procs_per_server = ARGV
raise "expected min < median < max" unless min < median and median < max

num_files = num_servers * procs_per_server
elements_per_file = num_elements / num_files

files = (0...num_servers).collect { |n| File.open("server.#{n}.sh","w") }

server = 0
num_files.times do |file|
	seed = Time.now.to_i + file
	files[server].puts "./generate_test_data_single.rb #{min} #{median} #{max} #{elements_per_file} #{seed} > data.#{file} &"
	server += 1
	server = 0 if server == num_servers
end

files.each { |f| f.close }
