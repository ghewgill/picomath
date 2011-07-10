require("erf")
require("expm1")
require("phi")
require("normal_cdf_inverse")
require("gamma")
require("log_factorial")

while true do
    local s = io.read()
    if s == nil then
        break
    end
    _, _, f, x = string.find(s, "(%w+) (.+)")
    x = tonumber(x)
    if f == "erf" then
        print(erf(x))
    elseif f == "expm1" then
        print(expm1(x))
    elseif f == "phi" then
        print(phi(x))
    elseif f == "NormalCDFInverse" then
        print(normal_cdf_inverse(x))
    elseif f == "Gamma" then
        print(gamma(x))
    elseif f == "LogGamma" then
        print(log_gamma(x))
    elseif f == "LogFactorial" then
        print(log_factorial(x))
    else
        print("Unknown function: ", f)
        break
    end
    io.flush()
end
