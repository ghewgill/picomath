import scala.math

//See http://www.johndcook.com/normal_cdf_inverse.html
class NormalCDFInverse {
  //This is good for 0 < p \leq 0.5
  //This is the rational approximation for the
  //complementary cumulative distribution function
  def rationalApproximation(t: Double) = {
    // Abramowitz and Stegun formula 26.2.23.
    // The absolute value of the error should be less than 4.5 e-4.
    val c: Array[Double] = Array(2.515517, 0.802853, 0.010328)
    val d: Array[Double] = Array(1.432788, 0.189269, 0.001308)
    val numerator: Double = (c(2)*t + c(1))*t + c(0);
    val denominator: Double = ((d(2)*t + d(1))*t + d(0))*t + 1.0;

    t - numerator / denominator
  }

  //Only defined for 0 < p < 1
  def normalCDFInverse(p: Double): Double = {
    require(p > 0.0 && p < 1)
    // See article above for explanation of this section.
    if (p < 0.5) {
      // F^-1(p) = - G^-1(p)
      return -rationalApproximation( math.sqrt(-2.0*math.log(p)) );
    } else {
      // F^-1(p) = G^-1(1-p)
      return rationalApproximation( math.sqrt(-2.0*math.log(1.0-p)) );
    }
  }
}

object NormalCDFInverse extends NormalCDFInverse
