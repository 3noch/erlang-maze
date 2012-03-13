-module(mapper).
-export([home/0, explore/1, print_maze/1]).

explore(Map) ->
    receive
        {start, From, Start, Goal} ->
            {A1, A2, A3} = now(),
            random:seed(A1, A2, A3),
            case track(Map, Start, Goal) of
                {ok, PathMap} -> From ! {ok, PathMap};
                {failed, PathMap} -> From ! {failed, PathMap}
            end;
        {'EXIT', _} -> true
    end.

home() ->
    Map = easy_maze:new(),
    start(Map),
    receive
        {ok, PathMap} ->
            io:format("Solution Found!~n"),
            print_maze(PathMap);
        {failed, _} ->
            io:format("Attempt failed.~n"),
            home()
    end.

start(Map) ->
    Pid = spawn(fun () -> explore(Map) end),
    Pid ! {start, self(), {21, 6}, {21, 20}}.


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


track(Map, Cell, Goal) when Cell == Goal -> {ok, Map};
track(Map, Cell, Goal) ->
    timer:sleep(100),
    print_maze(Map),
    OpenNeighbors = neighbors_with_tile(Map, Cell, ' '),
    case length(OpenNeighbors) of
        0 -> {failed, Map};
        _ ->
            RandomIndex = random:uniform(length(OpenNeighbors)),
            PickedNeighbor = lists:nth(RandomIndex, OpenNeighbors),
            NewMap = set_cell(Map, Cell, '.'),
            track(NewMap, PickedNeighbor, Goal)
    end.
