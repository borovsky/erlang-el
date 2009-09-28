%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2009, Alexander Borovsky
%%% @doc EL expressions scanner
%%%
%%% @end
%%% Created : 27 Sep 2009 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------
-module(erlang_el_scanner).

%% API
-export([scan/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Scans expression and transforms it to token list
%% @spec scan(string()) ->
%%           {ok, list(tuple())} | {error, string()}
%% @end
%%--------------------------------------------------------------------
-spec(scan(string()) ->
             {ok, list(tuple())} | {error, string()}).
scan(Text) ->
    scan(Text, [], in_expression).


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(scan(string(), list(tuple()), atom()) ->
             {ok, list(tuple())} | {error, string()}).
scan([], Parsed, in_expression) ->
    {ok, lists:reverse(Parsed)};

scan([], [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    scan([], [{identifier, lists:reverse(CollectedIdentifier)} | ScannedTail], in_expression);

scan([], [{integer, CollectedNumber}| ScannedTail], in_integer) ->
    Number = lists:reverse(CollectedNumber),
    try list_to_integer(Number) of
        Integer -> scan([], [{integer, Integer} | ScannedTail], in_expression)
    catch
        error:badarg -> {error, io_lib:format("Incorrect integer: ~p", [Number])}
    end;

scan([], [{float, CollectedNumber}| ScannedTail], in_float) ->
    Number = lists:reverse(CollectedNumber),
    try list_to_float(Number) of
        Float -> scan([], [{float, Float} | ScannedTail], in_expression)
    catch
        error:badarg -> {error, io_lib:format("Incorrect float: ~p", [Number])}
    end;

scan([$. | T], Scanned, in_expression) ->
    scan(T, [{dot} | Scanned], in_expression);

scan([$. | T], [{integer, CollectedIdentifier}| ScannedTail], in_integer) ->
    scan(T, [{float, [$.| CollectedIdentifier]} | ScannedTail], in_float);

scan([$. | _] = In, [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    scan(In, [{identifier, lists:reverse(CollectedIdentifier)} | ScannedTail], in_expression);

scan([$: | _] = In, [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    scan(In, [{identifier, lists:reverse(CollectedIdentifier)} | ScannedTail], in_expression);

scan("\"" ++ T, Scanned, in_expression) ->
    scan(T, [{string, ""} | Scanned], in_string);

scan("\\\""  ++ T, [{string, String} | Scanned], in_string) ->
    scan(T, [{string, "\"" ++ String} | Scanned], in_string);

scan("\""  ++ T, [{string, String} | Scanned], in_string) ->
    scan(T, [{string, lists:reverse(String)} | Scanned], in_expression);

scan([H | T], [{string, String} | Scanned], in_string) ->
    scan(T, [{string, [H | String]} | Scanned], in_string);

scan([H | T], Scanned, in_expression) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, [H]}| Scanned], in_identifier);
        digit -> scan(T, [{integer, [H]}| Scanned], in_integer);
        undefined -> {error, io_lib:format("Unknown char in expression: '~p'", [H])}
    end;

scan([H | T], [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    case char_type(H) of
        undefined -> {error, io_lib:format("Unknown char in identifier: '~p'", [H])};
        _ -> scan(T, [{identifier, [H | CollectedIdentifier]} | ScannedTail], in_identifier)
    end;

scan([H | T], [{integer, CollectedNumber}| ScannedTail], in_integer) ->
    case char_type(H) of
        digit -> scan(T, [{integer, [H | CollectedNumber]} | ScannedTail], in_integer);
        _ -> {error, io_lib:format("Unknown char in integer: '~p'", [H])}
    end;

scan([H | T], [{float, CollectedNumber}| ScannedTail], in_float) ->
    case char_type(H) of
        digit -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], in_float);
        _ -> case H of
                 $- -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], in_float);
                 $+ -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], in_float);
                 $e -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], in_float);
                 $E -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], in_float);
                 _ -> {error, io_lib:format("Unknown char in float: '~p'", [H])}
             end
    end.


%%--------------------------------------------------------------------
%% @doc Calculates character type
%% @spec char_type(char()) -> is_letter_underscope | digit | undefined
%% @end
%%--------------------------------------------------------------------
-spec(char_type(char()) -> letter_underscore | digit | undefined).
char_type(Char) ->
    case Char of 
        C when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) or (C == $_) ->
            letter_underscore;
        C when ((C >= $0) and (C =< $9)) ->
            digit;
        _ ->
            undefined
    end.
