%%%
%%% File    : erlang_el_SUITE.erl
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% Description : Test for Erlang-el
%%%
-module(erlang_el_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [
     variable_from_context,
     number
    ].

test_evaluate(Expected, Expression, Context) ->
    Expected = erlang_el:evaluate(Expression, Context).

variable_from_context(_Config) ->
    test_evaluate(1, "test", [{"test", 1}]).

number(_Config) ->
    test_evaluate(42, "42", [{"test", 1}]).

