public class Erf {
    public static double erf(double x) {
        // constants
        final double a1 =  0.254829592;
        final double a2 = -0.284496736;
        final double a3 =  1.421413741;
        final double a4 = -1.453152027;
        final double a5 =  1.061405429;
        final double p  =  0.3275911;

        // Save the sign of x
        double sign = 1;
        if (x < 0) {
            sign = -1;
        }
        x = Math.abs(x);

        // A&S formula 7.1.26
        double t = 1.0/(1.0 + p*x);
        double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.exp(-x*x);

        return sign*y;
    }
}
