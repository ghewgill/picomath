class test {
    public static void Main(string[] args) {
        while (true) {
            string s = System.Console.ReadLine();
            if (s == null) {
                break;
            }
            string[] a = s.Split();
            double x = System.Double.Parse(a[1]);
            if (a[0] == "erf") {
                System.Console.WriteLine(Erf.erf(x));
            } else if (a[0] == "expm1") {
                System.Console.WriteLine(Expm1.expm1(x));
            } else if (a[0] == "phi") {
                System.Console.WriteLine(Phi.phi(x));
            } else if (a[0] == "NormalCDFInverse") {
                System.Console.WriteLine(NormalCDFInverse.normalCDFInverse(x));
            } else if (a[0] == "Gamma") {
                System.Console.WriteLine(Gamma.gamma(x));
            } else if (a[0] == "LogGamma") {
                System.Console.WriteLine(Gamma.logGamma(x));
            } else {
                System.Console.WriteLine("Unknown funtion: " + a[0]);
                break;
            }
        }
    }
}
