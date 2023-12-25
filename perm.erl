-module(perm).
-compile(export_all).

perm(   []) -> [];
perm(  [I]) -> [[I]];
perm([I|L]) -> dist(I,perm(L)).

dist(_,          []) -> [];
dist(I,[Perm|Perms]) -> join(ins(I,Perm),dist(I,Perms)).
    
ins(I,        []) -> [[I]];
ins(I,Perm=[J|L]) -> [[I|Perm]|push(J,ins(I,L))].

push(_,          []) -> [];
push(I,[Perm|Perms]) -> [[I|Perm]|push(I,Perms)].

join(   [],Q) -> Q;
join([I|P],Q) -> [I|join(P,Q)].
