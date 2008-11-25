#!/usr/bin/env ruby
# ec2-describe-instances | ruby setup_users.rb
commands = [
	'useradd -m mat',
	'mv /root/mats_authorized_keys /home/mat/.ssh/authorized_keys',
	'chmod 644 /home/mat/.ssh/authorized_keys',
	'mkdir /mnt/data',
	'chown mat:mat /home/mat/.ssh/authorized_keys /mnt/data',
]
STDIN.readlines.
	select { |l| l =~ /INSTANCE/ }.
	collect { |l| l.split("\t")[3] }.each do |instance|
		puts "scp -i ~/dev/ec2/id_rsa-gsg-keypair ~/.ssh/authorized_keys root@#{instance}:mats_authorized_keys"
		puts "ssh -i ~/dev/ec2/id_rsa-gsg-keypair root@#{instance} '#{commands.join(';')}'"
end


