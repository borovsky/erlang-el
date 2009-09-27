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
-export([compile/2, evaluate/2]).

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


%%%===================================================================
%%% Internal functions
%%%===================================================================
process_expression(Expression, Context, Func) ->
    case erlang_el_scanner:scan(Expression) of
        {ok, Scanned} ->
            case erlang_el_parser:parse(Scanned) of
                {ok, Parsed} ->
                    Func(Parsed, Context);
                 Error -> Error
             end;
        Error -> Error
    end.
    

evaluate_parsed([], _) ->
    [];

evaluate_parsed({identifier, Identifier}, Context) ->
    erlang_el_runtime:get_value(Identifier, Context);

evaluate_parsed({number, Number}, _Context) ->
    Number.

compile_parsed(_Expression, _Context) ->
    erl_syntax:atom(ok).
