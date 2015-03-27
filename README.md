# parallant #

[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/parallant.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/parallant)

### Compilation ###

`make all`


### Running ###

There is a script that runs `algorithm_seq` and `algorithm_tiled` versions with both available models: `model_langton` and `model_forams` versions:

`./test.sh Width Height Depth PopulationSize Iterations`

or simply:

`make test` - to run with default parameters (20, 10, 1, 10, 100)

You can run also another configuration using erlang shell:

`1> parallant:start(Width, Height, Depth, PopulationSize, Steps, Options)`

where:

* `Width          :: pos_integer()` - width of the world
* `Height         :: pos_integer()` - height of the world
* `Depth          :: pos_integer()` - depth of the world
* `PopulationSize :: pos_integer() < Width*Height*Depth` - how many agents
* `Steps          :: pos_integer()` - how many simulation steps
* `Options        :: proplists:proplist()` - e.g. `[{model, model_langton}, {algorithm, algorithm_seq}, {log_world, false}]` - Run the simulation of a sequential algorithm with Langton's Ant model and don't log the state of every simulation step. The full list of possible options is described below.
  * `{algorithm, Algorithm}   :: algorithm_seq | algorithm_tiled`
  * `{model, Model}           :: model_langton | model_forams` - `model_langton` represents Langton's Ant simulation whereas `model_forams` is a simple algae dispersion in a grid simulation
  * `{agents, AgentsImpl}     :: agents_lists | agents_gbtree | agents_ets` - how agents (cells) are stored (using lists, a tree from `gbtree` module or an `ETS` table).
  * `{log_world, Log}         :: true | false` - the simulation state of every step is displayed if `true` (only the first and the last if `false`)
  * `{custom_log_interval, K} :: pos_integer()` - `log_custom/3` callback from given model is called every `K` iterations
  * `{animate, Animate}       :: true | false` - if `true` every logged step overwrites the previous one (might be not supported by the shell)


### Writing your own model ###

In order to run your own model, you should provide a module implementing a behaviour `model`.
Therefore, you need to implement four callback functions:
* `initial_population/3` - returns initial population in a form of a list of tuples of position and desired state
* `move/3` - takes a position of a current agent and current environment and returns updated environment
* `get_agent_char/2` - takes an agent state and returns a character that represents this agent on the displayed grid.
* `log_custom/3` - this function is called every `custom_log_interval` (set in config), it may be used to log some custom statistics based on the config record and current environment

Current agents' states can be fetched and updated by means of functions from the `agents` module:
  * `agents:get_agent/3` - returns current agent state on given position or `empty` if there is no agent on this position
  * `agents:update_agent/4` - updates given position with given agent state; pass empty as the agent_state to delete the agent; returns updated environment


Examples of this behaviour: `model_langton`, `model_forams`

In order to run it, you should provide the name of your module in the `Options` proplist. e.g. `[{model, my_custom_model}]`.
