#!/bin/sh
models="mas_hybrid"
cores="1"
split_sizes="16"
totaltime="600000"
runs=10

#models="mas_skel"
#cores="64"
#runs=1

Width=100
Height=50
Depth=20
Steps=10
# overall 100000

Cores="1"

Populations="10 100 1000 10000 50000"

for agents in $Populations; do
    ./run_zeus.sh $Width $Height $Depth $agents $Steps $Cores
done
