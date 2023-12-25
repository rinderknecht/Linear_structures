-module(isort).
-export([isort/1]).

isort(   []) -> [];
isort([I|L]) -> insert(I,isort(L)).

insert(I,[J|S]) when I > J -> [J|insert(I,S)];
insert(I,    T)            -> [I|T].
