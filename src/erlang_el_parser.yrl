%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright (C) 2009, Alexander Borovsky
%%% @doc Parser for erlang-el
%%%
%%% @end
%%% Created : 27 Sep 2009 by Alexander Borovsky <alex.borovsky@gmail.com>
%%%-------------------------------------------------------------------

Nonterminals
  Expression. 

Terminals
  identifier
  integer
  float
  string
  dot.

Rootsymbol
    Expression.

Expression -> '$empty' : [].
Expression -> identifier : '$1'.
Expression -> integer : '$1'.
Expression -> float : '$1'.
Expression -> string : '$1'.
Expression -> Expression dot identifier : {attribute, '$1', '$3'}.
