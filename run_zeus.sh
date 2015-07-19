#!/bin/bash

#!/bin/bash

######################
# use case parameters
######################

DefaultWidth=100
DefaultHeight=50
DefaultDepth=3
DefaultAgents=100
DefaultSteps=10
DefaultCores="1 2 4 8 12"

Width=${1:-$DefaultWidth}
Height=${2:-$DefaultHeight}
Depth=${3:-$DefaultDepth}
Agents=${4:-$DefaultAgents}
Steps=${5:-$DefaultSteps}
# 6th argument - Cores list

algorithms="algorithm_seq algorithm_tiled"
agents_impl="agents_ets"
models="model_forams model_langton3d" # modle_langton

tiles_per_colour=4
workers_per_colour=4


######################
# zeus parameters
######################


GrantID=plganiel2014b
Queue=l_short

CPU=X5650
ScriptRoot="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
OutputDir="output"
ScriptOutputDir="$ScriptRoot/$OutputDir"

Cores=${6:-$DefaultCores}
Time="$(($Steps * 120))"

# Common Zeus settings
CommonSettings=""
CommonSettings+=" ""-j oe" # Join stdout and stderr
CommonSettings+=" ""-A $GrantID"	# Grant ID
CommonSettings+=" ""-l walltime=$(($Time * 2))" # 2 times the job time
CommonSettings+=" ""-l pmem=512mb" # Memory per core
CommonSettings+=" ""-q $Queue" # Queue

######
# run
######

mkdir -p $ScriptOutputDir

for algorithm in $algorithms; do
    for model in $models; do
        for core in $Cores; do
            Command="echo '#$Width,$Height,$Depth,$Agents,$Steps,$algorithm,$model,$agents_impl'"
            Command+="\n"
            Command+=" ""erl -pa $ScriptRoot/ebin -pa $ScriptRoot/deps/*/ebin -eval 'parallant:start($Width,$Height,$Depth,$Agents,$Steps,[{algorithm,$algorithm},{model,$model},{agents,$agents_impl},{custom_log_interval,off},{log_world,false},{tiles_per_colour,$tiles_per_colour},{workers_per_colour,$workers_per_colour}]).' -run init stop -noshell"

            OutputFile="$ScriptOutputDir/$algorithm"_"$model"_"$core"_"$Steps"_"$Width"_"$Height"_"$Depth"_"$Agents.out"
            JobName="$algorithm"_"$model"_"$Agents"

            Settings=$CommonSettings
            Settings+=" ""-o $OutputFile"
            Settings+=" ""-N $JobName"
            Settings+=" ""-l nodes=1:$CPU:ppn=$core"

            echo "## running $Command"
            echo "## with settings $Settings"
            echo "## saving result in $OutputFile"
            echo -e "$Command" | qsub $Settings
        done
    done
done
