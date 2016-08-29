#!/usr/bin/env bash

# dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

find . -iname 'mintette-db*' -print | xargs rm -rfv
rm -rfv ~/.rscoin/{bank-db,mintette-db,wallet-db,notary-db,explorer-db}
