#!/usr/bin/env bash

# first argument stays for mintette you want to launch

test -z $1 && echo "No first argument" && exit

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

stack $NIX_STACK exec -- rscoin-bank --log-severity Debug add-mintette --host 127.0.0.1 --port 311$1 --key $(cat $dir/mintetteKeys/mintette$1/mintette$1.pub)
cd $dir/mintetteKeys/mintette$1/ 
stack $NIX_STACK exec -- rscoin-mintette --log-severity Debug --sk $dir/mintetteKeys/mintette$1/mintette$1.sec -p 311$1

