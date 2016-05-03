#!/usr/bin/env bash

# dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

find . -iname "mintette-db" -print | xargs rm -rfv
