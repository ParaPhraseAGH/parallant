-module(world_impl).

-include("parallant.hrl").

-export([create_board/3, update_board/3, update_cell/3, get_cell/3, display/3]).


% Callbacks

-callback create_board(dimension(), dimension(), config()) ->
    board().

-callback update_board(world(), [ant()], config()) ->
    world().

-callback update_cell(position(), world(), config()) ->
    world().

-callback get_cell(position(), world()) ->
    cell().

-callback display([ant()], world()) ->
    ok.

% Exported functions
-spec create_board(dimension(), dimension(), config()) -> board().
create_board(Width, Height, Config) ->
    Impl = Config#config.world_impl,
    Impl:create_board(Width, Height, Config).

-spec update_board(world_impl(), world(), [ant()]) -> world().
update_board(Impl, World, Ants) ->
    Impl:update_board(World, Ants).

-spec update_cell(position(), world(), config()) -> world().
update_cell(Pos, World, Config) ->
    Impl = Config#config.world_impl,
    Impl:update_cell(Pos, World, Config).

-spec get_cell(world_impl(), position(), world()) -> cell().
get_cell(Impl, Pos, World) ->
    Impl:get_cell(Pos, World).

-spec display(world_impl(), [ant()], world()) -> ok.
display(Impl, Ants, World) ->
    Impl:display(Ants, World).
