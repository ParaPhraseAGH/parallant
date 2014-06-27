#!/bin/bash

#Width=20
#Height=10
#Ants=2
#Steps=10

Width=$1
Height=$2
Ants=$3
Steps=$4

./rebar clean compile
echo ""
echo "version no_skel"
erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps)." -run init stop -noshell

#start(Model, Impl, Width, Height, PopulationSize, Steps)

./rebar clean compile -D skel
echo ""
echo "version skel"
erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps)." -run init stop -noshell