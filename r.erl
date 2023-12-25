-module(r).
-export([enqueue/2,dequeue/1,head/1]).

enqueue(I,{[], []}) -> {[],[I]};
enqueue(I,{In,Out}) -> {[I|In],Out}.

dequeue({In,    [I]}) -> {{[],rev(In)},I};
dequeue({In,[I|Out]}) -> {{In,Out},I}.
    
head({_,[I|_]}) -> I.     % Delay is 1

rev(L) -> rev_join(L,[]).

rev_join(   [],Q) -> Q;
rev_join([I|P],Q) -> rev_join(P,[I|Q]).
