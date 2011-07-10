require "erf.rb"
require "expm1.rb"
require "phi.rb"
require "normal_cdf_inverse.rb"
require "gamma.rb"
require "log_factorial.rb"

STDIN.each do |s|
    f, x = s.split
    x = x.to_f
    if f == "erf"
        print erf(x), "\n"
    elsif f == "expm1"
        print expm1(x), "\n"
    elsif f == "phi"
        print phi(x), "\n"
    elsif f == "NormalCDFInverse"
        print normal_cdf_inverse(x), "\n"
    elsif f == "Gamma"
        print gamma(x), "\n"
    elsif f == "LogGamma"
        print log_gamma(x), "\n"
    elsif f == "LogFactorial"
        print log_factorial(x), "\n"
    else
        STDERR.print "Unknown function: ", f, "\n"
        break
    end
    STDOUT.flush
end
