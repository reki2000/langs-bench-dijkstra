#!/bin/bash

set -e

function bench {
  local langs="$1"
  for count in 0 20; do
    hyperfine --warmup 1 -L lang "$langs" 'cd {lang}; ./bench.sh '${count}' < ../data/Tokyo_Edgelist.csv' --export-json out/result-${count}.json
    python plot_whisker.py --savefile out/result-${count}.png out/result-${count}.json
  done
}

function build {
  local lang=$1
  echo "Building $lang..."
  pushd $lang
  make -i clean && make
  popd
  echo ""
}

# create output directory
[ ! -d out ] && mkdir out

if [ -n "$*" ]; then
  langs="$*"
else
  langs="cpp go rust javascript julia kotlin python cython pypy dart"
fi

for lang in $(echo $langs); do
  build $lang
done
bench ${langs// /,}
