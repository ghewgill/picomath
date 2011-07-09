using System;

class NormalCDFInverse {

    static double RationalApproximation(double t)
    {
        // Abramowitz and Stegun formula 26.2.23.
        // The absolute value of the error should be less than 4.5 e-4.
        double[] c = {2.515517, 0.802853, 0.010328};
        double[] d = {1.432788, 0.189269, 0.001308};
        return t - ((c[2]*t + c[1])*t + c[0]) / 
                    (((d[2]*t + d[1])*t + d[0])*t + 1.0);
    }

    public static double normalCDFInverse(double p)
    {
        if (p <= 0.0 || p >= 1.0)
        {
            string msg = String.Format("Invalid input argument: {0}.", p);
            throw new ArgumentOutOfRangeException(msg);
        }

        // See article above for explanation of this section.
        if (p < 0.5)
        {
            // F^-1(p) = - G^-1(p)
            return -RationalApproximation( Math.Sqrt(-2.0*Math.Log(p)) );
        }
        else
        {
            // F^-1(p) = G^-1(1-p)
            return RationalApproximation( Math.Sqrt(-2.0*Math.Log(1.0 - p)) );
        }
    }

}
