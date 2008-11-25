N=$1
shift
set -x
erl -name $N@`hostname`.ec2.internal -setcookie A $*
