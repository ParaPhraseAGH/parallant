# parallant #


### Compilation ###

* Standard
  
```
#!bash

  ./rebar clean compile
```

* Debug 

  Every step of the simulation is displayed in an ASCII animation
```
#!bash
  
  ./rebar clean compile -D debug
```


### Running ###



```
#!bash

 erl -pa ebin -eval "parallant:test()." -s init stop -noshell

```

or in erlang shell:

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