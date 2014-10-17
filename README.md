# parallant #


### Compilation ###

* Standard
  
```
#!bash

  make all
```

* Skel

  Tiled version uses `skel` library

```
#!bash
  
  make skel
```


### Running ###

There is a script that runs `gbtree_based`: `seq`, `skel` and `not skel` versions:

```
#!bash

./test.sh Width Height PopulationSize Iterations

```

or


```
#!bash

 erl -pa ebin -eval "parallant:test()." -s init stop -noshell

```

You can run also another configuration using erlang shell:

```
#!erlang

1> parallant:start(Model, Impl, Width, Height, PopulationSize, Steps).

```

where: 

* `Model :: parallant_seq | parallant_tiled`
* `Impl :: list_based | gbtree_based`
* `Width :: pos_integer(), Height :: pos_integer()`
* `PopulationSize :: pos_integer() < Width*Height` - how many agents
* `Steps : pos_integer()` - number of simulation steps