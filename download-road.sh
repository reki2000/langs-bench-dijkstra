#!/bin/bash

curl https://ndownloader.figshare.com/files/3663336 > data/tokyo.zip
pushd data
unzip tokyo.zip
popd

Tokyo_EdgeList.csv

# XCoord,YCoord,START_NODE,END_NODE,EDGE,LENGTH
