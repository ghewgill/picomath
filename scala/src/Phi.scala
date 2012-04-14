import scala.math

class Phi {
  // constants
  val a1: Double =  0.254829592;
  val a2: Double = -0.284496736;
  val a3: Double =  1.421413741;
  val a4: Double = -1.453152027;
  val a5: Double =  1.061405429;
  val p: Double =  0.3275911;

    def phi(x: Double): Double = {
      // Save the sign of x
      val sign = if (x < 0) -1 else 1
      val absqx = math.abs(x)/math.sqrt(2.0);

      // A&S formula 7.1.26, rational approximation of error function
      val t: Double = 1.0/(1.0 + p*absqx);
      val y: Double = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*math.exp(-absqx*absqx);

      0.5*(1.0 + sign*y)
    }
}

object Phi extends Phi
