-module(ms_ave).
-export([means/1]).

means(0) -> [];
means(N) -> [mean(N)|means(N-1)].

mean(N) -> ave(sort(perm:perm(init(N)))).

sort(          []) -> [];
sort([Perm|Perms]) -> [snd(ms_c:sort(Perm))|sort(Perms)].
    
init(0) -> [];
init(N) -> [N|init(N-1)].

ave(L) -> {Sum,Len}=sum_len(L,0,0), Sum/Len.

sum_len(   [],Sum,Len) -> {Sum,Len};
sum_len([N|L],Sum,Len) -> sum_len(L,Sum+N,Len+1).

snd({_,Y}) -> Y.
