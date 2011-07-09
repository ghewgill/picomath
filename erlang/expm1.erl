-module(expm1).
-export([expm1/1]).

expm1(X) ->
    if
        abs(X) < 1.0e-5 -> X + 0.5*X*X;
        true            -> math:exp(X) - 1.0
    end.
