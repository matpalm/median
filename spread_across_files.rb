#!/usr/bin/env ruby

raise "spread_across_files file_prefix num_files" unless ARGV.length==2
file_prefix, num_files = ARGV
num_files = num_files.to_i

files = (0...num_files).collect { |i|	File.new("#{file_prefix}.#{i}","w") }

STDIN.each do |line|	
	file = files.shift
	file.puts line
	files << file
end

files.each { |f| f.close }

