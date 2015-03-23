#!/bin/bash

DefaultWidth=12 #100
DefaultHeight=8 #50
DefaultDepth=3
DefaultAgents=10 #100
DefaultSteps=10

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
#Depth=${3:-$DefaultDepth}
Agents=${4:-$DefaultAgents}
Steps=${5:-$DefaultSteps}

algorithms="algorithm_seq algorithm_tiled"
agents_impls="agents_lists agents_gbtree agents_ets"
models="model_langton model_langton3d model_forams"

tiles_per_colour=4
workers_per_colour=4

for algorithm in $algorithms; do
    for model in $models; do
        if [ $model = "model_langton3d" ] ; then
            Depth=${3:-$DefaultDepth} ; else
            Depth=1
        fi
    	for agents_impl in $agents_impls; do
        	echo ""
        	echo "# version $algorithm with $model and agents stored with $agents_impl"
        	erl -pa ebin -pa deps/*/ebin \
            	-eval "parallant:start($Width,$Height,$Depth,$Agents,$Steps,[{algorithm,$algorithm},{model,$model},{agents,$agents_impl},{log,false},{tiles_per_colour,$tiles_per_colour},{workers_per_colour,$workers_per_colour}])." \
            	-run init stop -noshell || exit 1
    	done
    done
done
