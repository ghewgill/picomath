-module(normal_cdf_inverse).
-export([normal_cdf_inverse/1]).

rational_approximation(T) ->
    % Abramowitz and Stegun formula 26.2.23.
    % The absolute value of the error should be less than 4.5 e-4.
    C0 = 2.515517, C1 = 0.802853, C2 = 0.010328,
    D0 = 1.432788, D1 = 0.189269, D2 = 0.001308,
    Numerator = (C2*T + C1)*T + C0,
    Denominator = ((D2*T + D1)*T + D0)*T + 1.0,
    T - Numerator / Denominator.

normal_cdf_inverse(P) ->
    % See article above for explanation of this section.
    if
        P < 0.5 ->
            % F^-1(p) = - G^-1(p)
            -rational_approximation( math:sqrt(-2.0*math:log(P)) );
        true ->
            % F^-1(p) = G^-1(1-p)
            rational_approximation( math:sqrt(-2.0*math:log(1.0-P)) )
    end.
