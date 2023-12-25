-module(q).
-export([perm_tf/1]).

perm_tf(L) -> perm(L,{}).

perm(   [],A) -> appk([],A);
perm(  [I],A) -> appk([[I]],A);
perm([I|L],A) -> perm(L,{k7,I,A}).

dist(_,      [],A) -> appk([],A);
dist(I,[Perm|P],A) -> dist(I,P,{k5,I,Perm,A}).
    
insert(I,        [],A) -> appk([[I]],A);
insert(I,Perm=[J|L],A) -> insert(I,L,{k3,[I|Perm],J,A}).

push(_,   [],A) -> appk([],A);
push(I,[L|H],A) -> push(I,H,{k124,[I|L],A}).

join(   [],Q,A) -> appk(Q,A);
join([I|P],Q,A) -> join(P,Q,{k124,I,A}).

appk(V,     {k7,I,A}) -> dist(I,V,A);
appk(V,     {k6,W,A}) -> join(V,W,A);
appk(V,{k5,I,Perm,A}) -> insert(I,Perm,{k6,V,A});
appk(V,   {k3,P,J,A}) -> push(J,V,{k124,P,A});
appk(V,   {k124,I,A}) -> appk([I|V],A);
appk(V,           {}) -> V.
