import sys

import erf
import expm1
import gamma
import log_one_plus_x
import normal_CDF_inverse
import phi
import log_factorial

Functions = {
    "erf": (erf.erf, float),
    "expm1": (expm1.expm1, float),
    "phi": (phi.phi, float),
    "NormalCDFInverse": (normal_CDF_inverse.normal_CDF_inverse, float),
    "Gamma": (gamma.gamma, float),
    "LogGamma": (gamma.log_gamma, float),
    "LogFactorial": (log_factorial.log_factorial, int),
}

while True:
    s = sys.stdin.readline()
    if s == "":
        break
    a = s.split()
    x = float(a[1])
    f = Functions.get(a[0])
    if f is None:
        sys.stderr.write("Unknown function: " + a[0] + "\n")
        break
    sys.stdout.write(str(f[0](f[1](float(a[1])))) + "\n")
    sys.stdout.flush()
