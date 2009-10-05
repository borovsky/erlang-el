%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2009, Alexander Borovsky
%%% @doc Parser for erlang-el
%%%
%%% @end
%%% Created : 27 Sep 2009 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------

Nonterminals
  Expression
  RootExpression
  ItemList
  .

Terminals
  identifier
  integer
  float
  string
  atom
  dot
  comma
  colon
  '['
  ']'
  '{'
  '}'
  '('
  ')'
  '=='
  '!='
  .

Rootsymbol
    RootExpression.

RootExpression -> Expression : '$1'.
RootExpression -> Expression comma ItemList : {list, ['$1' | '$3']}.

ItemList -> Expression : ['$1'].
ItemList -> Expression comma ItemList : ['$1' | '$3'].

Expression -> '$empty' : [].
Expression -> identifier : '$1'.
Expression -> integer : '$1'.
Expression -> float : '$1'.
Expression -> string : '$1'.
Expression -> atom : '$1'.
Expression -> Expression dot identifier : {attribute, '$1', '$3'}.
Expression -> '[' ItemList ']' : {list, '$2'}.
Expression -> '{' ItemList '}' : {tuple, '$2'}.
Expression -> identifier colon identifier '(' ItemList ')' : {call, '$1', '$3', '$5'}.

% Operators
Expression -> Expression '==' Expression : {is_equal, '$1', '$3'}.
Expression -> Expression '!=' Expression : {is_not_equal, '$1', '$3'}.
