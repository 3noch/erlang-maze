-module('mapper').
-export([track/3, print_maze/1]).


print_row([]) -> io:format("~n");
print_row([Char | Rest]) ->
    io:format(Char),
    print_row(Rest).


print_maze([]) -> ok;
print_maze([Row | Rest]) ->
    print_row(Row),
    print_maze(Rest).


set_cell(Map, {X, Y}, Value) ->
    OldRow = lists:nth(Y, Map),
    NewRow = lists:sublist(OldRow, X-1) ++ [Value | lists:nthtail(X, OldRow)],
    lists:sublist(Map, Y-1) ++ [NewRow | lists:nthtail(Y, Map)].


map_size(Map) -> {length(Map), length(lists:nth(1, Map))}.


neighbors(Map, {X, Y}) ->
    {Width, Height} = map_size(Map),
    Xs = if
            X == 1     -> [X+1];
            X == Width -> [X-1];
            true       -> [X-1, X+1]
        end,
    Ys = if
            Y == 1      -> [Y+1];
            Y == Height -> [Y-1];
            true        -> [Y-1, Y+1]
        end,
    [{A, Y} || A <- Xs] ++ [{X, B} || B <- Ys].


is_tile(Map, {X, Y}, Tile) ->
    lists:nth(X, lists:nth(Y, Map)) == Tile.


neighbors_with_tile(Map, Cell, Tile) ->
    [A || A <- neighbors(Map, Cell), is_tile(Map, A, Tile)].


track(Map, Cell, Goal) when Cell == Goal -> {success, Map};
track(Map, Cell, Goal) ->
    timer:sleep(1000),
    print_maze(Map),
    OpenNeighbors = neighbors_with_tile(Map, Cell, ' '),
    case length(OpenNeighbors) of
        0 -> {failure, Map};
        _ ->
            RandomIndex = random:uniform(length(OpenNeighbors)),
            PickedNeighbor = lists:nth(RandomIndex, OpenNeighbors),
            NewMap = set_cell(Map, Cell, '.'),
            track(NewMap, PickedNeighbor, Goal)
    end.
