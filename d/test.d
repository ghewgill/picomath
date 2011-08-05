import std.array;
import std.conv;
import std.mathspecial;
import std.stdio;

import log_factorial: log_factorial;

void main() {
    for (;;) {
        string s = readln();
        if (s == null) {
            break;
        }
        string[] a = split(s);
        string f = a[0];
        real x = to!real(a[1]);
        switch (f) {
            case "erf":
                writefln("%.15g", erf(x));
                break;
            case "expm1":
                writefln("%.15g", expm1(x));
                break;
            case "phi":
                writefln("%.15g", normalDistribution(x));
                break;
            case "NormalCDFInverse":
                writefln("%.15g", normalDistributionInverse(x));
                break;
            case "Gamma":
                writefln("%.15g", gamma(x));
                break;
            case "LogGamma":
                writefln("%.15g", lgamma(x));
                break;
            case "LogFactorial":
                writefln("%.15g", log_factorial(cast(uint)x));
                break;
            default:
                writeln("Unknown function: ", f);
                return;
        }
        stdout.flush();
    }
}
