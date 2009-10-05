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
    scan(Text, [], {1, 1}, in_expression).


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(scan(string(), list(tuple()), non_neg_integer(), atom()) ->
             {ok, list(tuple())} | {error, string()}).

%%%
%%% EOL parse
%%%
scan([], Parsed, _, in_expression) ->
    {ok, lists:reverse(Parsed)};

scan([], Parsed, {Row, Column}, Mode) ->
    scan(['$end'], Parsed, {Row, Column}, Mode);

%%%
%%% End of token process
%%%
scan(['$end' | T], [{identifier, CollectedIdentifier}| ScannedTail], {Row, Column}, in_identifier) ->
    scan(T, [{identifier, {Row, Column - 1}, lists:reverse(CollectedIdentifier)} | ScannedTail], {Row, Column}, in_expression);

scan(['$end' | _], _, _, in_string) ->
    {error, "End of line while string processing"};

scan(['$end' | _], _, _, in_atom) ->
    {error, "End of line while atom processing"};

scan(['$end' | T], [{integer, CollectedNumber}| ScannedTail], {Row, Column}, in_integer) ->
    Number = lists:reverse(CollectedNumber),
    try list_to_integer(Number) of
        Integer -> scan(T, [{integer, {Row, Column - 1}, Integer} | ScannedTail], {Row, Column}, in_expression)
    catch
        error:badarg -> {error, io_lib:format("Incorrect integer: ~p", [Number])}
    end;

scan(['$end' | T], [{float, CollectedNumber}| ScannedTail], {Row, Column}, in_float) ->
    Number = lists:reverse(CollectedNumber),
    try list_to_float(Number) of
        Float -> scan(T, [{float, {Row, Column - 1}, Float} | ScannedTail], {Row, Column}, in_expression)
    catch
        error:badarg -> {error, io_lib:format("Incorrect float: ~p", [Number])}
    end;

%%%
%%% Strings process
%%%
scan("\"" ++ T, Scanned, {Row, Column}, in_expression) ->
    scan(T, [{string, ""} | Scanned], {Row, Column + 1}, in_string);

scan("\\\""  ++ T, [{string, String} | Scanned], {Row, Column}, in_string) ->
    scan(T, [{string, "\"" ++ String} | Scanned], {Row, Column + 2}, in_string);

scan("\""  ++ T, [{string, String} | Scanned], {Row, Column}, in_string) ->
    scan(T, [{string, {Row, Column}, lists:reverse(String)} | Scanned], {Row, Column + 1}, in_expression);

scan([H | T], [{string, String} | Scanned], {Row, Column}, in_string) ->
    scan(T, [{string, [H | String]} | Scanned], {Row, Column + 1}, in_string);

%%%
%%% Atoms process
%%%
scan("'" ++ T, Scanned, {Row, Column}, in_expression) ->
    scan(T, [{atom, ""} | Scanned], {Row, Column + 1}, in_atom);

scan("\\'"  ++ T, [{atom, Atom} | Scanned], {Row, Column}, in_atom) ->
    scan(T, [{string, "'" ++ Atom} | Scanned], {Row, Column + 2}, in_atom);

scan("\'"  ++ T, [{atom, Atom} | Scanned], {Row, Column}, in_atom) ->
    scan(T, [{atom, {Row, Column}, list_to_atom(lists:reverse(Atom))} | Scanned], {Row, Column + 1}, in_expression);

scan([H | T], [{atom, Atom} | Scanned], {Row, Column}, in_atom) ->
    scan(T, [{atom, [H | Atom]} | Scanned], {Row, Column + 1}, in_atom);


%%%
%%% Dot process
%%%
scan([$. | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{dot, {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$. | T], [{integer, CollectedIdentifier} | ScannedTail], {Row, Column}, in_integer) ->
    scan(T, [{float, [$.| CollectedIdentifier]} | ScannedTail], {Row, Column + 1}, in_float);

scan([$. | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

%%%
%%% Compare process
%%%
scan("==" ++ T, Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'==', {Row, Column}} | Scanned], {Row, Column + 2}, in_expression);

scan("==" ++ _ = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


scan("!=" ++ T, Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'!=', {Row, Column}} | Scanned], {Row, Column + 2}, in_expression);

scan("!=" ++ _ = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


%%%
%%% Spaces process
%%%
scan(" " ++ T, Scanned, {Row, Column}, in_expression) ->
    scan(T, Scanned, {Row, Column + 1}, in_expression);

scan(" " ++ _ = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


scan("\n" ++ T, Scanned, {Row, _Column}, in_expression) ->
    scan(T, Scanned, {Row + 1, 1}, in_expression);

scan("\n" ++ _ = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


%%%
%%% Comma process
%%%
scan([$, | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{comma, {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$, | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

%%%
%%% Colon process
%%%
scan([$: | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{colon, {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$: | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


%%%
%%% Braces process
%%%
scan([$[ | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'[', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);
         
scan([$[ | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

scan([$] | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{']', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$] | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

scan([${ | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'{', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);
         
scan([${ | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

scan([$} | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'}', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$} | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

scan([$( | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{'(', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);
         
scan([$( | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);

scan([$) | T], Scanned, {Row, Column}, in_expression) ->
    scan(T, [{')', {Row, Column}} | Scanned], {Row, Column + 1}, in_expression);

scan([$) | _] = In, Scanned, {Row, Column}, Mode) ->
    scan(['$end' | In], Scanned, {Row, Column}, Mode);


%%%
%%% Expression mode
%%%
scan([H | T], Scanned, {Row, Column}, in_expression) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, [H]}| Scanned], {Row, Column + 1}, in_identifier);
        digit -> scan(T, [{integer, [H]}| Scanned], {Row, Column + 1}, in_integer);
        undefined -> {error, io_lib:format("Unknown char in expression: '~p'", [H])}
    end;

%%%
%%% Identifier mode
%%%
scan([H | T], [{identifier, CollectedIdentifier} | ScannedTail], {Row, Column}, in_identifier) ->
    case char_type(H) of
        undefined -> {error, io_lib:format("Unknown char in identifier: '~p'", [H])};
        _ -> scan(T, [{identifier, [H | CollectedIdentifier]} | ScannedTail], {Row, Column + 1}, in_identifier)
    end;

%%%
%%% Integer mode
%%%
scan([H | T], [{integer, CollectedNumber}| ScannedTail], {Row, Column}, in_integer) ->
    case char_type(H) of
        digit -> scan(T, [{integer, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_integer);
        _ -> {error, io_lib:format("Unknown char in integer: '~p'", [H])}
    end;

%%%
%%% Float mode
%%%
scan([H | T], [{float, CollectedNumber}| ScannedTail], {Row, Column}, in_float) ->
    case char_type(H) of
        digit -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_float);
        _ -> case H of
                 $- -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_float);
                 $+ -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_float);
                 $e -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_float);
                 $E -> scan(T, [{float, [H | CollectedNumber]} | ScannedTail], {Row, Column + 1}, in_float);
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
