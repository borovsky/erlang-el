%%%
%%% File    : erlang_el_SUITE.erl
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% Description : Test for Erlang-el
%%%
-module(erlang_el_runtime_SUITE).

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
     variable_from_list,
     variable_from_list_unexistant,
     variable_from_dict,
     variable_from_dict_unexistant,
     variable_from_gb_tree,
     variable_from_gb_tree_unexistant
    ].

variable_from_list(_Config) ->
    42 = erlang_el_runtime:get_value("test", [{"test", 42}]).

variable_from_list_unexistant(_Config) ->
    undefined = erlang_el_runtime:get_value("test2", [{"test", 42}]).

variable_from_dict(_Config) ->
    Dict = dict:store("test", 42, dict:new()),
    42 = erlang_el_runtime:get_value("test", Dict).

variable_from_dict_unexistant(_Config) ->
    Dict = dict:store("test2", 42, dict:new()),
    undefined = erlang_el_runtime:get_value("test", Dict).

variable_from_gb_tree(_Config) ->
    Tree = gb_trees:enter("test", 42, gb_trees:empty()),
    42 = erlang_el_runtime:get_value("test", Tree).

variable_from_gb_tree_unexistant(_Config) ->
    Tree = gb_trees:enter("test2", 42, gb_trees:empty()),
    undefined = erlang_el_runtime:get_value("test", Tree).
