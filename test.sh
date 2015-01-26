#!/bin/bash

DefaultWidth=20
DefaultHeight=10
DefaultAgents=10
DefaultSteps=100

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Agents=${3:-$DefaultAgents}
Steps=${4:-$DefaultSteps}

algorithms="algorithm_seq algorithm_tiled"
agents_impls="agents_lists agents_gbtree agents_ets"
models="model_langton model_forams"

for algorithm in $algorithms; do
    for model in $models; do
    	for agents_impl in $agents_impls; do
        	echo ""
        	echo "# version $algorithm with $model and agents stored with $agents_impl"
        	erl -pa ebin -pa deps/*/ebin \
            	-eval "parallant:start($Width,$Height,$Agents,$Steps,[{algorithm,$algorithm},{model,$model},{agents_impl,$agents_impl},{log,false}])." \
            	-run init stop -noshell || exit 1
    	done
    done
done
