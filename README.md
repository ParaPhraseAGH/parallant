# parallant #

[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/parallant.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/parallant)

### Compilation ###

`make all`


### Running ###

There is a script that runs `gbtree_based`: `seq` and `tiled` versions:

`./test.sh Width Height PopulationSize Iterations`

or


`erl -pa ebin -eval "parallant:test()." -s init stop -noshell`

You can run also another configuration using erlang shell:

`1> parallant:start(Model, Impl, Width, Height, PopulationSize, Steps, Log)`

where: 

* `Model :: parallant_seq | parallant_tiled`
* `Impl :: list_based | gbtree_based`
* `Width :: pos_integer(), Height :: pos_integer()`
* `PopulationSize :: pos_integer() < Width*Height` - how many agents
* `Steps : pos_integer()` - number of simulation steps
* `Log : true | false` - the simulation state of every step is displayed if `true` (only the first and the last if `false`)


When running `parallant_tiled` the width of the world should be even.
