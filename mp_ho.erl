-module(mp_ho).
-compile(export_all).
%-export([find/2]).

%find(Word,Text) -> W=mk_fail(Word), find(W,Text,0,W).

find(_,     [],_,_) -> absent;
find(X,T=[_|U],J,W) ->
  case prefix(X,T,0) of
    yes        -> {factor,J};
    {no,_,0,_} -> find(W,U,J+1,W);
    {no,V,I,F} -> find(suffix(X,F),V,J+I,W)
  end.

prefix(       [],    _,_) -> yes;
prefix([{A,_}|X],[A|T],I) -> prefix(X,T,I+1);
prefix([{_,F}|_],    T,I) -> {no,T,I,F}.

suffix(    X,0) -> X;
suffix([_|X],I) -> suffix(X,I-1).

% Preprocessing maximum border lengths (failures)
%
fail(_,0) -> -1;
fail(X,I) -> 1 + fp(X,suffix(X,I-1),fail(X,I-1)).

fp(_,       _,-1) -> -1;
fp(X,Y=[XI|_], B) -> case suffix(X,B) of
                       [XI|_] -> B;
                            _ -> fp(X,Y,fail(X,B))
                     end.

%mk_fail(X)         -> mk_fail(X,X,0).
%mk_fail(   [],_,_) -> [];
%mk_fail([A|Y],X,I) -> [{A,fail(X,I)}|mk_fail(Y,X,I+1)].

edges(A,Fail) ->
  fun(Next) -> fun(U=[B|T],J) -> case B of A -> Next(T,J);
                                           _ -> Fail(U,J)
                                 end;
                  (     [],_) -> absent
               end
  end.

init(A) -> 
  fun(Next) -> 
    F=fun(F) -> fun([B|T],J) -> case B of A -> Next(T,J);
                                          _ -> (F(F))(T,J+1)
                                end;
                   (   [],_) -> absent
                end
      end,
    F(F)
  end.

pproc(X) -> pproc([],X,X,0).

pproc(R,   [],_,_) -> next(fun(_,J) -> {factor,J} end,R);
pproc(R,[A|Y],X,0) -> pproc([init(A)|R],Y,X,1);
pproc([Last|R],[A|Y],X,I) ->
  Offset=I-1-fail(X,I),
  case Offset of
    0 -> L=fun(F) -> Last(edges(A,F)) end, L(Last);
  | _ -> [F|_]=suffix(R,Offset),
         New=edges(A,F),
         pproc([,Last(New)|R],Y,X,I+1)
  end.

next(F,   []) -> F;
next(F,[I|L]) -> next(I(F),L).

find(Word,Text) -> (pproc(Word))(Text,0).

%mk_next(  [A]) -> [edge(A,fun(_,J) -> {factor,J} end)];
%mk_next([A|X]) -> W=[Next|_]=mk_next(X), [edge(A,Next)|W].

