-module(ms_c).
-compile(export_all).        % So we can test all functions

sort([]) -> {[],1};
sort( L) -> {S,C}=solo(L,1), all(S,C).

all([P],C) -> {P,C+1};
all(  L,C) -> {S,D}=level(L,C+1), all(S,D).

level([P,Q|S],C) -> {R,D}=merge(P,Q,C+1), {T,E}=level(S,D),
                    {[R|T],E};
level(      L,C) -> {L,C+1}.

merge(   [],      Q,C)            -> {Q,C+1};
merge(    P,     [],C)            -> {P,C+1};
merge([I|P],Q=[J|_],C) when I < J -> {M,D}=merge(P,Q,C+1),
                                     {[I|M],D};
merge(    P,  [J|Q],C)            -> {M,D}=merge(P,Q,C+1),
                                     {[J|M],D}.

solo(   [],C) -> {[],C+1};
solo([I|L],C) -> {S,D}=solo(L,C+1), {[[I]|S],D}.
