import math

def expm1(x):
    if abs(x) < 1e-5:
        return x + 0.5*x*x
    else:
        return math.exp(x) - 1.0
