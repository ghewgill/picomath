-module(phi).
-export([phi/1]).

phi(X) ->
    % constants
    A1 =  0.254829592,
    A2 = -0.284496736,
    A3 =  1.421413741,
    A4 = -1.453152027,
    A5 =  1.061405429,
    P  =  0.3275911,

    % Save the sign of x
    Sign = if
        X >= 0 ->  1;
        true   -> -1
    end,
    A = abs(X)/math:sqrt(2.0),

    % A&S formula 7.1.26
    T = 1.0/(1.0 + P*A),
    Y = 1.0 - (((((A5*T + A4)*T) + A3)*T + A2)*T + A1)*T*math:exp(-A*A),

    0.5*(1.0 + Sign*Y).
