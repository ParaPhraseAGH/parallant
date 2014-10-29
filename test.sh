#!/bin/bash

DefaultWidth=20
DefaultHeight=10
DefaultAnts=10
DefaultSteps=100

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Ants=${3:-$DefaultAnts}
Steps=${4:-$DefaultSteps}

echo ""
echo "# version seq"
erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_seq, gbtree_based,$Width,$Height,$Ants,$Steps,false)." -run init stop -noshell

echo ""
echo "# version tiled no_skel"
erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps,false)." -run init stop -noshell

#start(Model, Impl, Width, Height, PopulationSize, Steps)

# make clean
# ./rebar clean compile -D skel
# echo ""
# echo "# version tiled skel"
# erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps,false)." -run init stop -noshell
