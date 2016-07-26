#!/usr/bin/env bash

# first argument stays for mintette you want to launch

test -z $1 && echo "No first argument" && exit

test $1 -ge 100 -o $1 -lt 0 && echo "No mintette number within range [0..99] is allowed" && exit 

if [ $1 -lt 10 ]; then
  mintNum="0$1"
else
  mintNum=$1
fi

if uname -a | grep -i nixos > /dev/null; then
  export NIX_STACK="--nix"
fi

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mpath=$dir/mintetteKeys/mintette$mintNum

if [ -z $2 ]; then
  sleep 2 && rscoin-bank --log-severity Debug add-mintette --host 127.0.0.1 --port 31$mintNum --key $(cat $mpath/mintette$mintNum.pub)&
  rscoin-mintette --log-severity Debug --path "$mpath/mintette-db" --sk $mpath/mintette$mintNum.sec -p 31$mintNum
else
  sleep 2 && stack $NIX_STACK exec rscoin-bank -- --log-severity Debug add-mintette --host 127.0.0.1 --port 31$mintNum --key $(cat $mpath/mintette$mintNum.pub)&
  stack $NIX_STACK exec rscoin-mintette -- --log-severity Debug --path "$mpath/mintette-db" --sk $mpath/mintette$mintNum.sec -p 31$mintNum
fi
