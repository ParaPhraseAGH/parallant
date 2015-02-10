#!/bin/bash

DefaultWidth=20
DefaultHeight=10
DefaultDepth=3
DefaultAgents=10
DefaultSteps=100

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Depth=${3:-$DefaultDepth}
Agents=${4:-$DefaultAgents}
Steps=${5:-$DefaultSteps}

algorithms="algorithm_seq algorithm_tiled"
agents_impls="agents_lists agents_gbtree agents_ets"
models="model_langton model_forams model_langton3d"

tiles_per_colour=4
workers_per_colour=4

for algorithm in $algorithms; do
    for model in $models; do
    	for agents_impl in $agents_impls; do
        	echo ""
        	echo "# version $algorithm with $model and agents stored with $agents_impl"
        	erl -pa ebin -pa deps/*/ebin \
            	-eval "parallant:start($Width,$Height,$Depth,$Agents,$Steps,[{algorithm,$algorithm},{model,$model},{agents_impl,$agents_impl},{log,false},{tiles_per_colour,$tiles_per_colour},{workers_per_colour,$workers_per_colour}])." \
            	-run init stop -noshell || exit 1
    	done
    done
done
