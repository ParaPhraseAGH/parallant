#!/bin/sh

# langton3d
Width=216
Height=100
Depth=20
Steps=10

#intel
Cores="1 2 4 8 12"

#opteron
Cores="1 4 8 16 32 48 64"

Cores="1 4"

Populations="200000"

#forams
# Width=768
# Height=200
# Depth=3


for agents in $Populations; do
    ./run_zeus.sh $Width $Height $Depth $agents $Steps "$Cores"
done
