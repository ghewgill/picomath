-module(test).

main([]) ->
    loop().

loop() ->
    S = io:get_line(""),
    [Func, X] = string:tokens(string:strip(S, right, $\n), " "),
    {V, []} = string:to_float(X),
    io:format("~f~n", [func(Func, V)]),
    loop().

func("erf", X) -> erf:erf(X);
func("expm1", X) -> expm1:expm1(X);
func("phi", X) -> phi:phi(X);
func("NormalCDFInverse", X) -> normal_cdf_inverse:normal_cdf_inverse(X);
func("Gamma", X) -> gamma:gamma(X);
func("LogGamma", X) -> gamma:log_gamma(X);
func("LogFactorial", X) -> log_factorial:log_factorial(trunc(X)).
