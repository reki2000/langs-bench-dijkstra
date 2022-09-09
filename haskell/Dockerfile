from ubuntu:jammy

WORKDIR /app

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y install curl

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN ghcup install ghc 9.2.4 --set 
RUN ghcup install cabal 3.8.1.0 --set 

COPY * ./

RUN cabal build
