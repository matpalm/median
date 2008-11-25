EC2_DESCRIBE_INSTANCES_OUTPUT = '.ec2-instances'

#role :servers, 
#	'ec2-67-202-33-100.compute-1.amazonaws.com',
#	'ec2-67-202-60-67.compute-1.amazonaws.com'

def call_describe_instances
	`ec2-describe-instances >> #{EC2_DESCRIBE_INSTANCES_OUTPUT}`
end

def ec2_instances
	instance_lines = File.read(EC2_DESCRIBE_INSTANCES_OUTPUT).split("\n").select {|line| line =~ /INSTANCE/ }
	external_addrs = instance_lines.collect { |i| i.split("\t")[3] }
end

call_describe_instances unless File.exists? EC2_DESCRIBE_INSTANCES_OUTPUT
role :servers, *ec2_instances

task :hostname_check do
	run "hostname"
end

task :update_instances do
	`rm #{EC2_DESCRIBE_INSTANCES_OUTPUT}`
end

task :copy_erlang_hosts_file do
	puts "calling :copy_hosts_file"
	# build hosts.erlang file 
	instance_lines = File.read(EC2_DESCRIBE_INSTANCES_OUTPUT).split("\n").select {|line| line =~ /INSTANCE/ }
	hosts = File.open('hosts.erlang.generated','w')
	internal_addrs = instance_lines.collect { |i| i.split("\t")[4] }
	internal_addrs.each {|a| hosts.puts "'#{a}'."}
	hosts.puts # required!
	hosts.close
	# copy it
	put File.read('hosts.erlang.generated'), '.hosts.erlang'
end

task :copy_scripts do
	# ruby generate_generate_scripts.rb 1 10 100 1e6 2 8
	`tar zcf scripts.tgz generate_test_data_single.rb erl.sh server*sh`
	copy 'scripts.tgz'
	run 'tar zxf scripts.tgz; chmod +x *sh *rb'
end

task :deploy do
	`tar zcf code.tgz *erl`
	copy 'code.tgz'
	run 'tar zxf code.tgz; erlc *erl'
end

namespace :erl do
	task :start do
	 run "sh erl.sh worker -detached"
	end
	task :stop do
	 run "killall beam.smp epmd"
	end
end

def copy file
 put File.read(file), file
end






