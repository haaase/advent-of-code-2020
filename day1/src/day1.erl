-module(day1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> none().
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    Path = get_filepath(Args),
    InputNumbers = strings_to_ints(Path),
    io:format("Solution1: ~p~n", [solve_part1(InputNumbers)]),
    io:format("Solution2: ~p~n", [solve_part2(InputNumbers)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

% generates the file path to be read
-spec get_filepath(string() | none()) -> [nonempty_string()].
get_filepath([Filename|_]) ->
    read_file(Filename);
get_filepath(_) ->
    read_file("input.txt").

% consumes a filepath and returns a list of number strings read from that file.
-spec read_file(string()) -> [nonempty_string()].
read_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    string:tokens(binary_to_list(Binary), "\n").

% takes a list of integer strings and turns it into a list of ints.
-spec strings_to_ints([nonempty_string()]) -> ['error' | integer()].
strings_to_ints(List) ->
    lists:map(fun(X) -> {Int,_} = string:to_integer(X), Int end, List).

% an expense report is a list of numbers
-spec solve_part1([integer()]) -> integer().
solve_part1(Numbers) ->
    Solutions = [{X,Y} || X <- Numbers, Y <- Numbers, X =/= Y, X+Y == 2020],
    case Solutions of
        [] ->
            -1;
        [{X,Y}|_] ->
            X*Y
        end.

-spec solve_part2([integer()]) -> integer().
solve_part2(Numbers) ->
    Solutions = [{X,Y,Z} || X <- Numbers, Y <- Numbers, Z <- Numbers, X =/= Y, X =/= Z, Y =/= Z, X+Y+Z == 2020],
    case Solutions of
        [] ->
            -1;
        [{X,Y,Z}|_] ->
            X*Y*Z
        end.