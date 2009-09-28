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
     variable_from_context,
     integer,
     float,
     attributes,
     string
    ].

variable_from_context(_Config) ->
    test_evaluate(1, "test", [{"test", 1}]).

integer(_Config) ->
    test_evaluate(42, "42", [{"test", 1}]).

float(_Config) ->
    test_evaluate(42.0e3, "42.0e3", [{"test", 1}]).

attributes(_Config) ->
    test_evaluate(22, "parent.child", [{"parent", [{"child", 22}]}]).

string(_Config) ->
    test_evaluate("Hello World!", "\"Hello World!\"", []).


%%%===================================================================
%%% Tests life support system
%%%===================================================================

test_evaluate(Expected, Expression, Context) ->
    Expected = erlang_el:evaluate(Expression, Context),
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
    Ast = erlang_el:compile(Expression, ContextAst),

    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(erlang_el_test)]),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(calculate), erl_syntax:integer(1))])]),
    CalculateFunctionAst = erl_syntax:function(erl_syntax:atom(calculate),
        [erl_syntax:clause([ContextAst], none, 
            [Ast])]),  
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, CalculateFunctionAst]].   

