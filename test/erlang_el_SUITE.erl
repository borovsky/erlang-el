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

%%%===================================================================
%%% Tests
%%%===================================================================

all() -> 
    [
     variable_from_context_eval,
     integer_eval,
     float_eval,
     attributes_eval,
     string_eval,
     atom_eval,
     root_list_eval,
     list_eval,
     tuple_eval,
     function_eval,

     equal_eval,
     not_equal_eval,

     not_eval
    ].

variable_from_context_eval(_Config) ->
    test_evaluate(1, "test", [{"test", 1}]).

integer_eval(_Config) ->
    test_evaluate(42, "42", [{"test", 1}]).

float_eval(_Config) ->
    test_evaluate(42.0e3, "42.0e3", [{"test", 1}]).

attributes_eval(_Config) ->
    test_evaluate(22, "parent.child", [{"parent", [{"child", 22}]}]).

string_eval(_Config) ->
    test_evaluate("Hello World!", "\"Hello World!\"", []),
    test_evaluate("Hello\nWorld!", "\"Hello\\nWorld!\"", []).

atom_eval(_Config) ->
    test_evaluate(test_atom, "'test_atom'", []).

root_list_eval(_Config) ->
    test_evaluate([1, 2.3, a, "b", 42], "1, 2.3, 'a', \"b\", c", [{"c", 42}]).

list_eval(_Config) ->
    test_evaluate([1, 2.3, a, "b", 42], "[1, 2.3, 'a', \"b\", c]", [{"c", 42}]).

tuple_eval(_Config) ->
    test_evaluate({1, 2.3, a, "b", 42}, "{1, 2.3, 'a', \"b\", c}", [{"c", 42}]).

function_eval(_Config) ->
    test_evaluate({1, 2.3, a, "b", 42},
                  "erlang:list_to_tuple([1, 2.3, 'a', \"b\", c])",
                  [{"c", 42}]).

equal_eval(_Config) ->
    test_evaluate(true, "1 == 1", []),
    test_evaluate(false, "1 == 2", []),
    test_evaluate(true, "'a' == 'a'", []),
    test_evaluate(false, "'a' == 'b'", []),
    test_evaluate(true, "'a' == erlang:list_to_atom(\"a\")", []).
    

not_equal_eval(_Config) ->
    test_evaluate(false, "1 != 1", []),
    test_evaluate(true, "1 != 2", []),
    test_evaluate(false, "'a' != 'a'", []),
    test_evaluate(true, "'a' != 'b'", []),
    test_evaluate(false, "'a' != erlang:list_to_atom(\"a\")", []).

not_eval(_Config) ->
    test_evaluate(false, "!1", []),
    test_evaluate(false, "!var", [{"var", true}]),
    test_evaluate(true, "!var", [{"var", false}]),
    test_evaluate(true, "!var", []).

%%%===================================================================
%%% Tests life support system
%%%===================================================================

test_evaluate(Expected, Expression, Context) ->
    {ok, Expected} = erlang_el:evaluate(Expression, Context),
    compile_module(Expression),
    Expected = erlang_el_test:calculate(Context).

compile_module(Expression) ->
    Forms = generate_module(Expression),
    case compile:forms(Forms, [report]) of
        {ok, Module1, Bin} -> 
            case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                {module, _} -> {ok, Module1, Bin};
                _ -> {error, lists:concat(["code reload failed: ", Module1])}
            end;
        error ->
            {error, lists:concat(["compilation failed"])};
        OtherError ->
            OtherError
    end.

generate_module(Expression) ->
    ContextAst = erl_syntax:variable("Context"),
    {ok, Ast} = erlang_el:compile(Expression, ContextAst),

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(erlang_el_test)]),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(calculate), erl_syntax:integer(1))])]),
    CalculateFunctionAst = erl_syntax:function(erl_syntax:atom(calculate),
        [erl_syntax:clause([ContextAst], none, 
            [Ast])]),  
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, CalculateFunctionAst]].   

