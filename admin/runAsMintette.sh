#!/usr/bin/env bash

# first argument stays for mintette you want to launch

test -z $1 && echo "No first argument" && exit

if uname -a | grep -i nixos > /dev/null; then
  export NIX_STACK="--nix"
fi

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mpath=$dir/mintetteKeys/mintette$1/

if [ -z $2 ]; then
  sleep 2 && rscoin-bank --log-severity Debug add-mintette --host 127.0.0.1 --port 311$1 --key $(cat $mpath/mintette$1.pub)&
  rscoin-mintette --log-severity Debug --path "$mpath/mintette-db" --sk $mpath/mintette$1.sec -p 311$1
else
  sleep 2 && stack $NIX_STACK exec rscoin-bank -- --log-severity Debug add-mintette --host 127.0.0.1 --port 311$1 --key $(cat $mpath/mintette$1.pub)&
  stack $NIX_STACK exec rscoin-mintette -- --log-severity Debug --path "$mpath/mintette-db" --sk $mpath/mintette$1.sec -p 311$1
fi
