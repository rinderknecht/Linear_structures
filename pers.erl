-module(pers).
-export([push/2,pop/1,ver/2,change/3]).

push(I,{N,H}) -> {N+1,[{push,I}|H]}.
pop({N,H}) -> {top(H),{N-1,[pop|H]}}.

top(H)              -> top(0,H).
top(0,[{push,I}|_]) -> I;
top(K,[{push,_}|H]) -> top(K-1,H);
top(K,     [pop|H]) -> top(K+1,H).

ver(K,{N,H}) when K >= 0 -> ver__(K,N,H).
ver__(0,N,           H)  -> last(0,N,H);
ver__(K,N,     [pop|H])  -> ver__(K-1,N+1,H);
ver__(K,N,[{push,_}|H])  -> ver__(K-1,N-1,H).

last(_,0,           _) -> [];
last(0,N,[{push,I}|H]) -> [I|last(0,N-1,H)];
last(M,N,[{push,_}|H]) -> last(M-1,N-1,H);
last(M,N,     [pop|H]) -> last(M+1,N+1,H).

change(K,U,{N,H}) when K >= 0 ->
                         {D,H1} = chg(K,U,N,H,N), {N+D,H1}.

chg(0,U,_,             H,M)            -> repl(U,H,M);
chg(K,U,N,       [pop|H],M)            ->
             {N1,H1} = chg(K-1,U,N+1,H,  M), {N1,[pop|H1]};
chg(K,U,N,[P={push,_}|H],M) when M < N -> 
             {N1,H1} = chg(K-1,U,N-1,H,  M), {N1,  [P|H1]};
chg(K,U,N,[P={push,_}|H],_)            ->
             {N1,H1} = chg(K-1,U,N-1,H,N-1), {N1,  [P|H1]}.

repl(       pop,   H=[pop|_],_)            -> { 0,      H};
repl(P={push,_},[{push,_}|H],_)            -> { 0,  [P|H]};
repl(P={push,_},     [pop|H],_)            -> { 2,  [P|H]};
repl(       pop,[{push,_}|H],M) when M > 1 -> {-2,[pop|H]}.
