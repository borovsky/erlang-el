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

scan([], [{number, CollectedNumber}| ScannedTail], in_number) ->
    scan([], [{number, list_to_integer(lists:reverse(CollectedNumber))} | ScannedTail], in_expression);

scan([H | T], Scanned, in_expression) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, [H]}| Scanned], in_identifier);
        digit -> scan(T, [{number, [H]}| Scanned], in_number)
    end;

scan([$: | _] = In, [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    scan(In, [{identifier, lists:reverse(CollectedIdentifier)} | ScannedTail], in_expression);

scan([H | T], [{identifier, CollectedIdentifier}| ScannedTail], in_identifier) ->
    case char_type(H) of
        undefined -> {error, "Unknown char in identifier"};
        _ -> scan(T, [{identifier, [H | CollectedIdentifier]} | ScannedTail], in_identifier)
    end;

scan([H | T], [{number, CollectedNumber}| ScannedTail], in_number) ->
    case char_type(H) of
        digit -> scan(T, [{number, [H | CollectedNumber]} | ScannedTail], in_number);
        _ -> {error, "Unknown char in number"}
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
