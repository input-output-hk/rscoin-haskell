#!/bin/sh

# first argument stays for mintette you want to launch

test -z $1 && echo "No first argument" && exit

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

stack $NIX_STACK exec -- rscoin-bank --log-severity Debug add-mintette --host localhost --port 311$1 --key $(cat $dir/mintetteKeys/mintette$1/mintette$1.pub)
cd $dir/mintetteKeys/mintette$1/ 
stack $NIX_STACK exec -- rscoin-user --sk $dir/mintetteKeys/mintette$1/mintette$1.sec -p 311$1

