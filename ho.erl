-module(ho).
-export([max_f/3,max_x/3]).

% Question 1
%
% The test A > B ensures that A and B are integers
%
max_int(A,B) when A =< B -> B;
max_int(A,B) when A > B  -> A.

max_f(F,B,B)            -> F(B);
max_f(F,A,B) when A < B -> max_int(F(A),max_f(F,A+1,B)).

% Question 2
%
max_x(_,B,B) -> B;
max_x(F,A,B) when A < B -> M = max_x(F,A+1,B),
                           case F(A) >= M of
                             true  -> A;
                             false -> M
                           end.

