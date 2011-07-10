function rational_approximation(t)
    -- Abramowitz and Stegun formula 26.2.23.
    -- The absolute value of the error should be less than 4.5 e-4.
    c = {2.515517, 0.802853, 0.010328}
    d = {1.432788, 0.189269, 0.001308}
    numerator = (c[3]*t + c[2])*t + c[1]
    denominator = ((d[3]*t + d[2])*t + d[1])*t + 1.0
    return t - numerator / denominator
end

function normal_cdf_inverse(p)
    assert(p > 0.0 and p < 1, "Invalid input")

    -- See article above for explanation of this section.
    if p < 0.5 then
        -- F^-1(p) = - G^-1(p)
        return -rational_approximation( math.sqrt(-2.0*math.log(p)) )
    else
        -- F^-1(p) = G^-1(1-p)
        return rational_approximation( math.sqrt(-2.0*math.log(1.0-p)) )
    end
end
