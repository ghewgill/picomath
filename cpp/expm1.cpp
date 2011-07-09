#include <cmath>

// Compute exp(x) - 1 without loss of precision for small values of x.
double expm1(double x)
{
    if (fabs(x) < 1e-5)
        return x + 0.5*x*x;
    else
        return exp(x) - 1.0;
}
