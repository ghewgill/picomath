import math

# compute log(1+x) without loss of precision for small values of x
def log_one_plus_x(x):
    if x <= -1.0:
        raise FloatingPointError("argument must be > -1")

    if abs(x) > 1e-4:
        # x is large enough that the obvious evaluation is OK
        return math.log(1.0 + x)
    else:
        # Use Taylor approx. 
        # log(1 + x) = x - x^2/2 with error roughly x^3/3
        # Since |x| < 10^-4, |x|^3 < 10^-12, 
        # and the relative error is less than 10^-8
        return (-0.5*x + 1.0)*x
