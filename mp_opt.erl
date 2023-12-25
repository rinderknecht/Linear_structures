-module(mp_opt).
-export([find/2]).

find(Word,Text) -> W=mk_fail(Word), find(W,Text,0,W).

find(_,     [],_,_) -> absent;
find(X,T=[_|U],J,W) ->
  case prefix(X,T,0) of
    yes        -> {factor,J};
    {no,_,0,_} -> find(W,U,J+1,W);
    {no,V,I,F} -> find(suffix(X,F),V,J+I-F,W)
  end.

prefix(       [],    _,_) -> yes;
prefix([{A,_}|X],[A|T],I) -> prefix(X,T,I+1);
prefix([{_,F}|_],    T,I) -> {no,T,I,F}.

suffix(    X,0) -> X;
suffix([_|X],I) -> suffix(X,I-1).

% Preprocessing maximum side lengths (failures)
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

mk_fail(     []) -> [];
mk_fail(X=[A|Y]) -> Fst={A,-1}, [Fst|mk_fail(Y,X,Fst)].

mk_fail(   [],_,       _) -> [];
mk_fail([A|Y],X,{B,Fail}) -> Cur = {A,1 + fp(X,B,Fail)},
                             [Cur|mk_fail(Y,X,Cur)].
