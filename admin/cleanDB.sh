#!/bin/sh

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

find $dir -iname "mintette-db" -print | xargs rm -rfv
