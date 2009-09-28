%%%
%%% File    : erlang_el_SUITE.erl
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% Description : Test for Erlang-el
%%%
-module(erlang_el_scaner_SUITE).

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
     parse_identifier,
     parse_integer,
     parse_float
    ].

test_parse(Expected, Expression) ->
    Parsed = erlang_el_scanner:scan(Expression),
    Expected = Parsed.

parse_identifier(_Config) ->
    test_parse({ok, [{identifier, "test"}]}, "test").

parse_integer(_Config) ->
    test_parse({ok, [{number, 42}]}, "42").

parse_float(_Config) ->
    test_parse({ok, [{number, 42.0e3}]}, "42.0e3").
