# langs-bench-dijkstra
Simple benchmarks of Dijkstra algorithm among C++, Go, Julia, Python, JavaScript, Rust and Kotlin 

# Requirement

This benchmark uses [hyperfine](https://github.com/sharkdp/hyperfine).

And runs on languages below:
- Go : 1.14
- Rust : 1.44
- JavaScript : NodeJS 13 
- Kotlin : 1.3 + jdk >=8
- Julia : 1.4

I like to use [asdf](https://asdf-vm.com/#/) to set up those envirionments.

```setup.sh
while read lang plugin version dummy; do
  asdf plugin add $plugin
  asdf install $plugin $version
  [ "$lang" != "-" ] && (cd $lang; asdf local $plugin $version)
  popd 
done <<EOT
go golang 1.14.2
python python 3.8.2
- java adopt-openjdk-14.0.1+7
kotlin kotlin 1.3.72
rust rust 1.44.0
julia julia 1.4.1
javascript nodejs 13.13.0
EOT
```

Also needs to get the Tokyo's road network data from [Urban Road Network Data](https://figshare.com/articles/Urban_Road_Network_Data/2061897)
```
curl https://ndownloader.figshare.com/files/3663336 > data/tokyo.zip
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
./run.sh [cpp|go|rust|javascript|julia|kotlin]
```
