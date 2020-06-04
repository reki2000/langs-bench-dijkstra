#!/bin/bash

function bench {
  export N=40
  for count in 1 5; do
    hyperfine --warmup 1 "COUNT=$count make run"
  done
}

function measure {
  local lang=$1
  echo "Testing $lang..."
  pushd $lang > /dev/null
  make clean > /dev/null && make > /dev/null
  bench
  popd > /dev/null
}
set -e

if [ -n "$1" ]; then
  measure $1
else 
  for lang in cpp go rust javascript julia kotlin; do
    measure $lang
  done
fi