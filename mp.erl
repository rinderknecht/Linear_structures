-module(mp).
-export([find/2]).

find(Word,Text) -> find(Word,Text,0,Word).

find(_,     [],_,_) -> absent;
find(X,T=[_|U],J,W) ->
  case prefix(X,T,0) of
    yes      -> {factor,J};
    {no,_,0} -> find(W,U,J+1,W);
    {no,V,I} -> F=fail(X,I), find(suffix(X,F),V,J+I-F,W)
  end.

prefix(   [],    _,_) -> yes;
prefix([A|X],[A|T],I) -> prefix(X,T,I+1);
prefix(    _,    T,I) -> {no,T,I}.

suffix(    X,0) -> X;
suffix([_|X],I) -> suffix(X,I-1).

% Maximum side lengths (failure function)
%
fail(_,0) -> -1;
fail(X,I) -> 1 + fp(X,nth(X,I-1),fail(X,I-1)).

nth([A|_],0) -> A;
nth([_|X],N) -> nth(X,N-1).

fp(_,_,  -1) -> -1;
fp(X,B,Fail) -> case nth(X,Fail) of
                  B -> Fail;
                  _ -> fp(X,B,fail(X,Fail))
                end.
