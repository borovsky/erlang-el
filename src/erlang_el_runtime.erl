%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2009, Alexander Borovsky
%%% @doc Runtime library for Erlang-EL
%%%
%%% @end
%%% Created : 27 Sep 2009 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(erlang_el_runtime).

%% API
-export([get_value/2, is_false/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Fetches value from map structure
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec(get_value(atom(), gb_tree() | dict() | list() | tuple()) -> 
             any()).
get_value(Key, List) when is_list(List) ->
    proplists:get_value(Key, List);

get_value(Key, {GBSize, _} = Tree) when is_integer(GBSize) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Val} ->
            Val;
        _ ->
            undefined
    end;

get_value(Key, Tuple) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        Module ->
            KeyAtom = list_to_atom(Key),
            case proplists:get_value(KeyAtom, Module:module_info(exports)) of
                1 ->
                    Tuple:KeyAtom();
                _ ->
                    undefined
            end
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================
is_false(false) ->
    true;

is_false(undefined) ->
    true;

is_false(_) ->
    false.
