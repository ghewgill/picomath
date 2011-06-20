using System;

class Expm1 {

    public static double expm1(double x)
    {
        if (Math.Abs(x) < 1e-5)
            return x + 0.5*x*x;
        else
            return Math.Exp(x) - 1.0;
    }

}
