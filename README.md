# langs-bench-dijkstra
Simple benchmarks of Dijkstra algorithm among C++, Go, Julia, Python(+Cython), JavaScript, Rust and Kotlin.

# Requirement

Submodules are contained.
You need to `git submodule update --init --recursive` at first.

This benchmark uses [hyperfine](https://github.com/sharkdp/hyperfine).

To plot the benchmark results, needs `pip install numpy matplotlib  # pip3, if you are using python3b`

And runs on languages below:
- Go : 1.14
- Rust : 1.44
- JavaScript : NodeJS 13 
- Kotlin : 1.3 + jdk >=8
- Julia : 1.4
- Clang : 7 (or versions which support C++17)
- Dart : 2.15
- Python : 3.8

I like using [asdf](https://asdf-vm.com/#/) to set up those environments, except Clang.

```setup.sh
while read lang plugin version dummy; do
  asdf plugin add $plugin
  asdf install $plugin $version
  (cd $lang; asdf local $plugin $version)
done <<EOT
go golang 1.14.2
python python 3.8.2
cython python 3.8.2
pypy python pypy3.6-7.3.1
kotlin java adopt-openjdk-14.0.1+7
kotlin kotlin 1.3.72
rust rust 1.44.0
julia julia 1.4.1
javascript nodejs 13.13.0
dart 2.15.1
EOT
asdf reshim
```

Also you need to get the Tokyo's road network data from [Urban Road Network Data](https://figshare.com/articles/Urban_Road_Network_Data/2061897) .
```
mkdir data
curl -L https://ndownloader.figshare.com/files/3663336 > data/tokyo.zip
pushd data
unzip tokyo.zip
popd
```

# How to run

for all languages
```
./run.sh
```

for specific language
```
./run.sh [cpp|go|rust|javascript|julia|kotlin|python|cython|pypy|dart]
```

for test setup
```
mkdir out
./test.sh cpp # choose one implementation to make 'correct' result
mv out/cpp.txt out/expect.txt
```

for test
```
./test.sh [cpp|go|rust|javascript|julia|kotlin|python|cython|pypy]
```
