# parallant #

[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/parallant.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/parallant)

### Compilation ###

`make all`


### Running ###

There is a script that runs `parallant_seq` algorithm with both available models: `model_langton` and `model_forams` versions:

`./test.sh Width Height PopulationSize Iterations`

or simply:

`make test` - to run with default parameters (20, 10, 10, 100)

You can run also another configuration using erlang shell:

`1> parallant:start(Width, Height, PopulationSize, Steps, Options)`

where: 

* `Width          :: pos_integer()` - width of the world
* `Height         :: pos_integer()` - height of the world
* `PopulationSize :: pos_integer() < Width*Height` - how many agents
* `Steps          :: pos_integer()` - how many simulation steps
* `Options        :: proplists:proplist()` - e.g. `[{model, model_langton}, {algorithm, parallant_seq}, {log, false}]` - Run the simulation of a sequential algorithm with Langton's Ant model and don't log the state of every simulation step. The full list of possible options is described below.
  * `{algorithm, Algorithm}   :: parallant_seq | parallant_tiled`
  * `{model, Model}           :: model_langton | model_forams` - `model_langton` represents Langton's Ant simulation whereas `model_forams` is a simple algae dispersion in a grid simulation
  * `{ants_impl, AntsImpl}    :: ants | ants_gbt | ants_ets` - how agents (cells) are stored. For now, the only option is `ants`. `ants_gbt` and `ants_ets` are proposed in #8
  * `{log, Log}               :: true | false` - the simulation state of every step is displayed if `true` (only the first and the last if `false`)
  * `{animate, Animate}       :: true | false` - if `true` every logged step overwrites the previous one (might be not supported by the shell)


### Writing your own model ###

In order to run your own model, you should provide a module implementing a behaviour `model`. 
Therefore, you need to implement three callback functions:
* `initial_population/4` - returns initial population in a form of a list of tuples of position and desired state
* `move/3` - takes a position of a current agent and current environment and returns updated environment
* `get_agent_char/2` - takes an agent state and returns a character that represents this agent on the displayed grid.

Examples of this behaviour: `model_langton`, `model_forams`

In order to run it, you should provide the name of your module in the `Options` proplist. e.g. `[{model, my_custom_model}]`.
