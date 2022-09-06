# langs-bench-dijkstra
Simple benchmarks of Dijkstra algorithm among C++, Go, Julia, Python(+Cython +PyPy), JavaScript(Node), Rust, Dart and Kotlin.

![result-20](https://user-images.githubusercontent.com/2533597/179347589-1102cfbf-78e5-4a40-a7a8-b4e4e4d82ebe.png)

# Setup

## Tools

This benchmark uses [hyperfine](https://github.com/sharkdp/hyperfine). Follow the install instruction there.


For `cpp` and `unregulated-cpp20` , submodules are contained. You need to

```
git submodule update --init --recursive
```

 at first.

To plot the benchmark results, you need matplotlib module.

```
pip install numpy matplotlib
```

## Language Environments

You need running environments for languages below:
- Go : 1.18
- Rust : 1.62
- JavaScript : NodeJS 18
- Kotlin : 1.7 + jdk >= 18
- Julia : 1.7
- Clang : 7 (or versions which support C++17)
- GCC(g++) : 10 (or versions which support C++20)
- Dart : 2.16.1
- Python : 3.10, Cython 0.29, PyPy 3.9-7.3.9
- Haskell: GHC 9.2.4

I like using [asdf](https://asdf-vm.com/#/) to set up those environments, except Clang and Haskell.

```setup.sh
while read lang plugin dummy; do
  asdf plugin add $plugin
  (cd $lang; asdf install)
done <<EOT
go golang
python python
cython python
pypy python
kotlin java
kotlin kotlin
rust rust
julia julia
javascript nodejs
dart dart
EOT
asdf reshim
```

for Haskell, prepare `ghc` and `cabal` by using [ghcup](https://www.haskell.org/ghcup/).

```
ghcup install ghc 9.2.4 --set 
ghcup install cabal 3.8.1.0 --set 
```

## Road Network Data

you need to get the Tokyo's road network data from [Urban Road Network Data](https://figshare.com/articles/Urban_Road_Network_Data/2061897) .
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
./run.sh [cpp|go|rust|javascript|julia|kotlin|python|cython|pypy|dart|unregulated-cpp20]
```

for test setup - choose one implementation (ex.`cpp`) to make a 'correct' result.
```
mkdir out
./test.sh cpp
mv out/cpp.txt out/expected.txt
```

for test
```
./test.sh [cpp|go|rust|javascript|julia|kotlin|python|cython|pypy|dart|unregulated-cpp20]
```
