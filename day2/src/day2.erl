-module(day2).

%% API exports
-export([main/1, parse_policy/1, read_file/1, check_policy/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    Input = read_file(Args),
    io:format("Solution1: ~p~n", [solve_part1(Input)]),
    io:format("Solution2: ~p~n", [solve_part2(Input)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

% reads the input file and gives back a list of string tokens
-spec read_file(string() | none()) -> [nonempty_string()].
read_file([Filename|_]) ->
    tokenize_file(Filename);
read_file(_) ->
    tokenize_file("input.txt").

% consumes a filepath and returns a list of number strings read from that file.
-spec tokenize_file(string()) -> [nonempty_string()].
tokenize_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    string:tokens(binary_to_list(Binary), "\n").


-record(pw_policy, {character :: char(),
                 min_times :: non_neg_integer(),
                 max_times :: non_neg_integer()}).
-type pw_policy() :: #pw_policy{}.

% consumes a line of the input string and returns a tuple of password policy and password
-spec parse_policy(string()) -> {pw_policy(), string()}.
parse_policy(Policy) ->
    case string:tokens(Policy, "- :") of 
        [Min, Max, Char, PW] ->
            {MinInt,_} = string:to_integer(Min),
            {MaxInt,_} = string:to_integer(Max),
            {#pw_policy{character=hd(Char), min_times=MinInt, max_times=MaxInt}, PW}
    end.

-spec check_policy(pw_policy(), nonempty_string()) -> boolean().
check_policy(Policy, Password) ->
    Times = length(lists:filter(fun(X) -> X == Policy#pw_policy.character end, Password)),
    (Times >= Policy#pw_policy.min_times) and (Policy#pw_policy.max_times >= Times). 

-spec solve_part1([string()]) -> non_neg_integer().
solve_part1(Lines) ->
    Checked_Lines = lists:map(fun(L) -> {Pol, Pass} = parse_policy(L), check_policy(Pol, Pass) end, Lines),
    length(lists:filter(fun(L) -> L =:= true end, Checked_Lines)).

-spec check_policy2(pw_policy(), nonempty_string()) -> boolean().
check_policy2(Policy, Password) ->
    Pos1 = lists:nth(Policy#pw_policy.min_times, Password) =:= Policy#pw_policy.character,
    Pos2 = lists:nth(Policy#pw_policy.max_times, Password) =:= Policy#pw_policy.character,
    Pos1 xor Pos2. % only return true if either position 1 or position 2 contain the requested char


-spec solve_part2([string()]) -> non_neg_integer().
solve_part2(Lines) ->
    Checked_Lines = lists:map(fun(L) -> {Pol, Pass} = parse_policy(L), check_policy2(Pol, Pass) end, Lines),
    length(lists:filter(fun(L) -> L =:= true end, Checked_Lines)).