#!/bin/bash

DefaultWidth=20
DefaultHeight=10
DefaultAnts=10
DefaultSteps=100

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Ants=${3:-$DefaultAnts}
Steps=${4:-$DefaultSteps}

algorithms="parallant_seq parallant_tiled"
world_impls="list_based gbtree_based"
ants_impls="ants" # ants_gbt
models="model_langton model_forams"

for algorithm in $algorithms; do
    for model in $models; do
    	for ants_impl in $ants_impls; do
        	echo ""
        	echo "# version $algorithm with $model and ants stored with $ants_impl"
        	erl -pa ebin -pa deps/*/ebin \
            	-eval "parallant:start($Width,$Height,$Ants,$Steps,[{algorithm,$algorithm},{model,$model},{ants_impl,$ants_impl},{log,false}])." \
            	-run init stop -noshell || exit 1
    	done
    done
done

# make clean
# ./rebar clean compile -D skel
# echo ""
# echo "# version tiled skel"
# erl -pa ebin -pa deps/skel/ebin -eval "parallant:start(parallant_tiled, gbtree_based,$Width,$Height,$Ants,$Steps,false)." -run init stop -noshell
