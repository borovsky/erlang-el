%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2009, Alexander Borovsky
%%% @doc
%%%
%%% @end
%%% Created : 26 Sep 2009 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(erlang_el).

%% API
-export([compile/2, compile/3, evaluate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Evaluates expression
%% @spec evaluate(string(), any()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(evaluate(string(), any()) -> any()).
evaluate(Expression, Context) ->
    process_expression(Expression, Context, fun evaluate_parsed/2).


%%--------------------------------------------------------------------
%% @doc Compiles expression to erlang
%% @spec compile(string(), any()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(compile(string(), any()) -> any()).
compile(Expression, ContextAst) ->
    process_expression(Expression, ContextAst, fun compile_parsed/2).

%%--------------------------------------------------------------------
%% @doc Compiles expression to erlang
%% @spec compile(string(), {non_neg_integer(), non_neg_integer()}, any()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec(compile(string(), {non_neg_integer(), non_neg_integer()}, any()) -> any()).
compile(Expression, {Row, Column} , ContextAst) ->
    process_expression(Expression, {Row, Column}, ContextAst, fun compile_parsed/2).


%%%===================================================================
%%% Internal functions
%%%===================================================================
process_expression(Expression, Position, Context, Func) ->
    case erlang_el_scanner:scan(Expression, Position) of
        {ok, Scanned} ->
            case erlang_el_parser:parse(Scanned) of
                {ok, Parsed} ->
                    process_parsed(Func, Parsed, Context);
                Error -> Error
            end;
        Error -> Error
    end.

process_expression(Expression, Context, Func) ->
    case erlang_el_scanner:scan(Expression) of
        {ok, Scanned} ->
            case erlang_el_parser:parse(Scanned) of
                {ok, Parsed} ->
                    process_parsed(Func, Parsed, Context);
                Error -> Error
            end;
        Error -> Error
    end.

process_parsed(Func, Parsed, Context) ->
    try Func(Parsed, Context) of
        Value -> {ok, Value}
    catch
        Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates parsed expression
%% @spec evaluate_parsed(list(tuple()), any()) -> any()
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_parsed(list(tuple()), any()) -> any()).
evaluate_parsed([], _) ->
    [];

evaluate_parsed({integer, _, Number}, _Context) ->
    Number;

evaluate_parsed({float, _, Number}, _Context) ->
    Number;

evaluate_parsed({string, _, String}, _Context) ->
    String;

evaluate_parsed({atom, _, Atom}, _Context) ->
    Atom;

evaluate_parsed({identifier, _, Identifier}, Context) ->
    erlang_el_runtime:get_value(Identifier, Context);

evaluate_parsed({attribute, Parent, {identifier, _, Child}}, Context) ->
    erlang_el_runtime:get_value(Child, evaluate_parsed(Parent, Context));

evaluate_parsed({is_equal, First, Second}, Context) ->
    evaluate_parsed(First, Context) == evaluate_parsed(Second, Context);

evaluate_parsed({is_not_equal, First, Second}, Context) ->
    not(evaluate_parsed({is_equal, First, Second}, Context));

evaluate_parsed({list, List}, Context) ->
    evaluate_list(List, Context);

evaluate_parsed({tuple, List}, Context) ->
    list_to_tuple(evaluate_list(List, Context));

evaluate_parsed({call, {identifier, _, ModuleName},
                 {identifier, _, FunctionName}, Args}, Context) -> 
    ProcessedArgs = evaluate_list(Args, Context),
    Module = list_to_atom(ModuleName),
    Function = list_to_atom(FunctionName),
    apply(Module, Function, ProcessedArgs);

evaluate_parsed({'not', Expression}, Context) ->
    erlang_el_runtime:is_false(evaluate_parsed(Expression, Context)).

evaluate_list(List, Context) ->
    lists:map(fun(I) -> evaluate_parsed(I, Context) end, List).


%%--------------------------------------------------------------------
%% @doc Compiles parsed expression
%% @spec compile_parsed(list(tuple()), any()) -> any()
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(compile_parsed(list(tuple()), any()) -> any()).
compile_parsed({integer, _, Number}, _ContextAst) ->
    erl_syntax:integer(Number);

compile_parsed({float, _, Number}, _ContextAst) ->
    erl_syntax:float(Number);

compile_parsed({string, _, String}, _ContextAst) ->
    erl_syntax:string(String);

compile_parsed({atom, _, Atom}, _ContextAst) ->
    erl_syntax:atom(Atom);

compile_parsed({identifier, _, Identifier}, ContextAst) ->
    erl_syntax:application(erl_syntax:atom(erlang_el_runtime),
                           erl_syntax:atom(get_value),
                           [erl_syntax:string(Identifier), ContextAst]);

compile_parsed({attribute, Parent, {identifier, _, Child}}, ContextAst) ->
    ParentAst = compile_parsed(Parent, ContextAst),
    erl_syntax:application(erl_syntax:atom(erlang_el_runtime),
                           erl_syntax:atom(get_value),
                           [erl_syntax:string(Child), ParentAst]);

compile_parsed({is_equal, First, Second}, ContextAst) ->
    FirstAst = compile_parsed(First, ContextAst),
    SecondAst = compile_parsed(Second, ContextAst),
    erl_syntax:infix_expr(FirstAst, erl_syntax:operator('==') ,SecondAst);

compile_parsed({is_not_equal, First, Second}, ContextAst) ->
    Ast = compile_parsed({is_equal, First, Second}, ContextAst),
    erl_syntax:application(erl_syntax:atom('erlang'),
                           erl_syntax:atom('not'),
                           [Ast]);


compile_parsed({list, List}, ContextAst) ->
    erl_syntax:list(compile_list(List, ContextAst));

compile_parsed({tuple, List}, ContextAst) ->
    erl_syntax:tuple(compile_list(List, ContextAst));

compile_parsed({call, {identifier, _, ModuleName},
                {identifier, _, FunctionName}, Args}, ContextAst) ->
    Module = erl_syntax:atom(ModuleName),
    Function = erl_syntax:atom(FunctionName),
    erl_syntax:application(Module, Function, compile_list(Args, ContextAst));

compile_parsed({'not', Expression}, ContextAst) ->
    Module = erl_syntax:atom(erlang_el_runtime),
    Function = erl_syntax:atom(is_false),
    erl_syntax:application(Module, Function, [compile_parsed(Expression, ContextAst)]).

compile_list(List, ContextAst) ->
    lists:map(fun(I) -> compile_parsed(I, ContextAst) end, List).
