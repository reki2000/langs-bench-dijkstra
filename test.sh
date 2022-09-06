#!/bin/bash

set -e    # exit when command failed
set -u    # exit when non-initalized variable found

function check {
  local lang=$1
  echo "Testing $lang..."
  pushd $lang > /dev/null
  make -i clean > /dev/null && make > /dev/null
  ./bench.sh 1 debug < ../data/Tokyo_Edgelist.csv > ../out/$lang.txt
  popd > /dev/null
  diff -u out/expected.txt out/$lang.txt | head
}

if [ -n "$1" ]; then
  for lang in "$@"; do
    check $lang
  done
else
  for lang in cpp go rust javascript julia kotlin python cython pypy dart haskell unregulated-cpp20; do
    check $lang
  done
fi
