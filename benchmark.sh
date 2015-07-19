#!/bin/sh

Width=256
Height=128
Depth=64
Steps=10
# overall 2^21

Cores="1"

Populations="128 1024 16384 131072 1048576"


# Width=100
# Height=20
# Width=3
# Steps=5
# Populations="10"


for agents in $Populations; do
    ./run_zeus.sh $Width $Height $Depth $agents $Steps $Cores
done
