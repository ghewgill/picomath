module NormalCdfInverse (normalCdfInverse) where

rational_approximation t = let
    -- Abramowitz and Stegun formula 26.2.23.
    -- The absolute value of the error should be less than 4.5 e-4.
    c = [2.515517, 0.802853, 0.010328]
    d = [1.432788, 0.189269, 0.001308]
    numerator = ((c!!2)*t + (c!!1))*t + (c!!0)
    denominator = (((d!!2)*t + (d!!1))*t + (d!!0))*t + 1.0
    in t - numerator / denominator

normalCdfInverse p | p > 0.0 && p < 1 =
    -- See article above for explanation of this section.
    if p < 0.5
        -- F^-1(p) = - G^-1(p)
        then -rational_approximation( sqrt(-2.0*log(p)) )
        -- F^-1(p) = G^-1(1-p)
        else rational_approximation( sqrt(-2.0*log(1.0-p)) )
