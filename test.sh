#!/bin/bash

DefaultWidth=20
DefaultHeight=10
DefaultAnts=10
DefaultSteps=100

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Ants=${3:-$DefaultAnts}
Steps=${4:-$DefaultSteps}

models="parallant_seq parallant_tiled"
backends="list_based gbtree_based"

for model in $models; do
    for backend in $backends; do
        echo ""
        echo "# version $model with $backend backend"
        erl -pa ebin -pa deps/skel/ebin \
            -eval "parallant:start($Width,$Height,$Ants,$Steps,[{algorithm,$model},{world_impl,$backend},{log,false}])." -run init stop -noshell
    done
done

# make clean
# ./rebar clean compile -D skel
# echo ""
# echo "# version tiled skel"
# erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps,false)." -run init stop -noshell
