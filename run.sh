#!/bin/bash

set -e

function bench {
  [ ! -d out ] && mkdir out
  hyperfine --warmup 1 -L lang "$1" 'cd {lang}; ./bench.sh 20 < ../data/Tokyo_Edgelist.csv' --export-json out/result.json
  python plot_whisker.py --labels "$1" --savefile out/result.png out/result.json 
}

function build {
  local lang=$1
  echo "Building $lang..."
  pushd $lang
  make -i clean && make
  popd
  echo ""
}

if [ -n "$1" ]; then
  langs="$1"
else
  langs="cpp go rust javascript julia kotlin python cython pypy"
fi

for lang in $(echo $langs); do
  build $lang
done
bench ${langs// /,}
