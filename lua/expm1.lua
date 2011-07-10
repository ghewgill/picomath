function expm1(x)
    if math.abs(x) < 1e-5 then
        return x + 0.5*x*x
    else
        return math.exp(x) - 1.0
    end
end
