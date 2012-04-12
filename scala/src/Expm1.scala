import scala.math

//Calculates the value of the natural logarithm base (e) raised to the power of x, minus 1.
class Expm1 {
    def expm1(x: Double): Double = {
        if (math.abs(x) < 1e-5) {
            return x + 0.5*x*x;
        } else {
            return math.exp(x) - 1.0;
        }
    }
}

object Expm1 extends Expm1
