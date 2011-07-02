import sys

import erf
import expm1
import gamma
import log_one_plus_x
import normal_CDF_inverse
import phi

Functions = {
    "erf": erf.erf,
    "expm1": expm1.expm1,
    "phi": phi.phi,
    "NormalCDFInverse": normal_CDF_inverse.normal_CDF_inverse,
    "Gamma": gamma.gamma,
    "LogGamma": gamma.log_gamma,
}

while True:
    s = sys.stdin.readline()
    if s == "":
        break
    a = s.split()
    f = Functions.get(a[0])
    if f is None:
        sys.stderr.write("Unknown function: " + a[0] + "\n")
        break
    sys.stdout.write(str(f(float(a[1]))) + "\n")
    sys.stdout.flush()
