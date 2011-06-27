importPackage(java.io);
importPackage(java.lang);

load("erf.js");
load("expm1.js");
load("phi.js");
load("normal_cdf_inverse.js");
load("gamma.js");

var stdin = new BufferedReader(new InputStreamReader(System['in']));
while (true) {
    var s = stdin.readLine();
    if (s === null) {
        break;
    }
    var a = s.split(" ");
    var f = a[0];
    var x = Number(a[1]);
    if (f == "erf") {
        print(erf(x));
    } else if (f == "expm1") {
        print(expm1(x));
    } else if (f == "phi") {
        print(phi(x));
    } else if (f == "NormalCDFInverse") {
        print(normal_cdf_inverse(x));
    } else if (f == "Gamma") {
        print(gamma(x));
    } else if (f == "LogGamma") {
        print(log_gamma(x));
    } else {
        System.err.println("Unknown function: " + f);
        break;
    }
}
