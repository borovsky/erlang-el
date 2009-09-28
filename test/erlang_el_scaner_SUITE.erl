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
     parse_float,
     parse_attribute,
     parse_string,
     parse_atom,
     parse_comma
    ].

test_parse(Expected, Expression) ->
    Parsed = erlang_el_scanner:scan(Expression),
    Expected = Parsed.

parse_identifier(_Config) ->
    test_parse({ok, [{identifier, "test"}]}, "test").

parse_integer(_Config) ->
    test_parse({ok, [{integer, 42}]}, "42").

parse_float(_Config) ->
    test_parse({ok, [{float, 42.0e3}]}, "42.0e3").

parse_attribute(_Config) ->
    test_parse({ok, [{identifier, "parent"}, {dot}, {identifier, "child"}]}, "parent.child").

parse_string(_Config) ->
    test_parse({ok, [{string, "test string"}]}, "\"test string\""),
    test_parse({ok, [{string, "test.string"}]}, "\"test.string\""),
    test_parse({ok, [{string, "test,string"}]}, "\"test,string\"").

parse_atom(_Config) ->
    test_parse({ok, [{atom, test_atom}]}, "'test_atom'"),
    test_parse({ok, [{atom, 'test atom'}]}, "'test atom'"),
    test_parse({ok, [{atom, 'test.atom'}]}, "'test.atom'"),
    test_parse({ok, [{atom, 'test,atom'}]}, "'test,atom'").

parse_comma(_Config) ->
    test_parse({ok, [{integer, 1}, {comma}, {integer, 2}]}, "1,2"),
    test_parse({ok, [{identifier, "a"}, {comma}, {identifier, "b"}]}, "a,b"),
    test_parse({ok, [{float, 3.1}, {comma}, {float, 4.16}]}, "3.1,4.16"),
    test_parse({ok, [{atom, a}, {comma}, {atom, b}]}, "'a','b'"),
    test_parse({ok, [{string, "a"}, {comma}, {string, "b"}]}, "\"a\",\"b\"").
