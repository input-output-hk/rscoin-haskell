#!/bin/sh

# first argument stays for mintette you want to launch

test -z $1 && echo "No first argument" && exit

alias rscoin-mintette="stack $NIX_STACK exec -- rscoin-mintette --log-severity Debug"
alias rscoin-bank="stack $NIX_STACK exec -- rscoin-bank --log-severity Debug"
alias rscoin-user="stack $NIX_STACK exec -- rscoin-user" 

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

rscoin-bank add-mintette --host localhost --port 311$1 --key $(cat $dir/mintetteKeys/mintette$1/mintette$1.pub)
rscoin-mintette --sk $dir/mintetteKeys/mintette$1/mintette$1.sec -p 311$1

