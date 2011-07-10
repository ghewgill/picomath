import java.io.DataInputStream;
import java.io.IOException;

class Test {
    public static void main(String[] args) {
        DataInputStream dis = new DataInputStream(System.in);
        while (true) {
            try {
                String s = dis.readLine();
                if (s == null) {
                    break;
                }
                String[] a = s.split(" ");
                double x = Double.parseDouble(a[1]);
                if (a[0].equals("erf")) {
                    System.out.println(Erf.erf(x));
                } else if (a[0].equals("expm1")) {
                    System.out.println(Expm1.expm1(x));
                } else if (a[0].equals("phi")) {
                    System.out.println(Phi.phi(x));
                } else if (a[0].equals("NormalCDFInverse")) {
                    System.out.println(NormalCDFInverse.normalCDFInverse(x));
                } else if (a[0].equals("Gamma")) {
                    System.out.println(Gamma.gamma(x));
                } else if (a[0].equals("LogGamma")) {
                    System.out.println(Gamma.logGamma(x));
                } else if (a[0].equals("LogFactorial")) {
                    System.out.println(LogFactorial.logFactorial((int)x));
                } else {
                    System.err.println("Unknown function: " + a[0]);
                    break;
                }
            } catch (IOException e) {
                System.err.println(e);
                break;
            }
        }
    }
}
