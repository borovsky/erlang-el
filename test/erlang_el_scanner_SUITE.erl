%%%
%%% File    : erlang_el_SUITE.erl
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% Description : Test for Erlang-el
%%%
-module(erlang_el_scanner_SUITE).

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
     parse_comma,
     parse_newline,
     parse_list,
     parse_tuple,
     parse_function_call,

     parse_equal,
     parse_not_equal,

     parse_not
    ].

test_parse(Expected, Expression) ->
    Parsed = erlang_el_scanner:scan(Expression),
    Expected = Parsed.

parse_identifier(_Config) ->
    test_parse({ok, [{identifier, {1, 4}, "test"}]}, "test").

parse_integer(_Config) ->
    test_parse({ok, [{integer, {1, 2}, 42}]}, "42").

parse_float(_Config) ->
    test_parse({ok, [{float, {1, 6}, 42.0e3}]}, "42.0e3").

parse_attribute(_Config) ->
    test_parse({ok, [{identifier, {1, 6}, "parent"},
                     {dot, {1, 7}},
                     {identifier, {1, 12}, "child"}]}, "parent.child").

parse_string(_Config) ->
    test_parse({ok, [{string, {1, 13}, "test string"}]}, "\"test string\""),
    test_parse({ok, [{string, {1, 13}, "test.string"}]}, "\"test.string\""),
    test_parse({ok, [{string, {1, 13}, "test,string"}]}, "\"test,string\""),
    test_parse({ok, [{string, {1, 14}, "test\nstring"}]}, "\"test\\nstring\"").

parse_atom(_Config) ->
    test_parse({ok, [{atom, {1, 11}, test_atom}]}, "'test_atom'"),
    test_parse({ok, [{atom, {1, 11}, 'test atom'}]}, "'test atom'"),
    test_parse({ok, [{atom, {1, 11}, 'test.atom'}]}, "'test.atom'"),
    test_parse({ok, [{atom, {1, 11}, 'test,atom'}]}, "'test,atom'").

parse_comma(_Config) ->
    test_parse({ok, [{integer, {1, 1}, 1},
                     {comma, {1, 2}},
                     {integer, {1, 3}, 2}]}, "1,2"),
    test_parse({ok, [{identifier, {1, 1}, "a"},
                     {comma, {1, 2}},
                     {identifier, {1, 3}, "b"}]}, "a,b"),
    test_parse({ok, [{float, {1, 3}, 3.1},
                     {comma, {1, 4}},
                     {float, {1,8}, 4.16}]}, "3.1,4.16"),
    test_parse({ok, [{atom, {1, 3}, a},
                     {comma, {1, 4}},
                     {atom, {1,7}, b}]}, "'a','b'"),
    test_parse({ok, [{string, {1,3}, "a"},
                     {comma, {1, 4}},
                     {string, {1,7}, "b"}]}, "\"a\",\"b\"").

parse_newline(_Config) ->
    test_parse({ok, [{integer, {1, 1}, 1},
                     {comma, {1, 2}},
                     {integer, {2, 1}, 2}]}, "1,\n2").

parse_list(_Config) ->
    test_parse({ok, [{'[', {1, 1}},
                      {integer, {1, 2}, 1},
                      {comma, {1, 3}},
                      {integer, {1, 5}, 2},
                      {']', {1, 6}}]}, "[1, 2]").

parse_tuple(_Config) ->
    test_parse({ok, [{'{', {1, 1}},
                      {integer, {1, 2}, 1},
                      {comma, {1, 3}},
                      {integer, {1, 5}, 2},
                      {'}', {1, 6}}]}, "{1, 2}").

parse_function_call(_Config) ->
    test_parse({ok, [{identifier, {1, 6}, "erlang"},
                     {colon, {1, 7}},
                     {identifier, {1, 20}, "list_to_tuple"},
                     {'(', {1, 21}},
                     {'[', {1, 22}},
                     {integer, {1, 23}, 1},
                     {comma, {1, 24}},
                     {integer, {1, 26}, 2},
                     {']', {1, 27}},
                     {')', {1, 28}}
                    ]}, "erlang:list_to_tuple([1, 2])").

parse_equal(_Config) ->
    test_parse({ok, [{integer, {1, 1}, 1},
                     {'==', {1, 3}},
                     {integer, {1, 6}, 2}]}, "1 == 2").

parse_not_equal(_Config) ->
    test_parse({ok, [{integer, {1, 1}, 1},
                     {'!=', {1, 3}},
                     {integer, {1, 6}, 2}]}, "1 != 2").

parse_not(_Config) ->
    test_parse({ok, [{'!', {1, 1}},
                     {integer, {1, 2}, 1}]}, "!1").
