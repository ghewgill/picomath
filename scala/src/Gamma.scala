import scala.math
import scala.annotation.tailrec

// Adapted from http://www.johndcook.com/stand_alone_code.html
// All bugs are however likely my fault
class Gamma {
  //Entry points
  def gamma(x:Double): Double = {
    hoboTrampoline(x,false,((y: Double) => y))
  }
  def logGamma(x:Double): Double = {
    hoboTrampoline(x,true,((y: Double) => y))
  }

  //Since scala doesn't support optimizing co-recursive tail-calls
  //we manually make a trampoline and make it tail recursive
  @tailrec
  private def hoboTrampoline(x: Double, log: Boolean,todo: Double => Double): Double = {
    if (!log) {
        if (x <= 0.0)
        {
            val msg = "Invalid input argument "+x+". Argument must be positive."
            throw new IllegalArgumentException(msg);
        }

        // Split the function domain into three intervals:
        // (0, 0.001), [0.001, 12), and (12, infinity)

        ///////////////////////////////////////////////////////////////////////////
        // First interval: (0, 0.001)
        //
        // For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
        // So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
        // The relative error over this interval is less than 6e-7.

        val gamma: Double = 0.577215664901532860606512090; // Euler's gamma constant
        if (x < 0.001) {
            1.0/(x*(1.0 + gamma*x));
        } else if (x < 12.0) {
          ///////////////////////////////////////////////////////////////////////////
          // Second interval: [0.001, 12)
          // The algorithm directly approximates gamma over (1,2) and uses
          // reduction identities to reduce other arguments to this interval.
          val arg_was_less_than_one: Boolean = (x < 1.0);

          // Add or subtract integers as necessary to bring y into (1,2)
          // Will correct for this below
          val (n: Integer,y: Double) =  if (arg_was_less_than_one)
            {
              (0,x + 1.0)
            } else {
              val n: Integer = x.floor.toInt - 1;
              (n,x-n)
            }

          // numerator coefficients for approximation over the interval (1,2)
          val p: Array[Double] =
            Array(
              -1.71618513886549492533811E+0,
              2.47656508055759199108314E+1,
              -3.79804256470945635097577E+2,
              6.29331155312818442661052E+2,
              8.66966202790413211295064E+2,
              -3.14512729688483675254357E+4,
              -3.61444134186911729807069E+4,
              6.64561438202405440627855E+4
            );

          // denominator coefficients for approximation over the interval (1,2)
          val q: Array[Double] =
            Array(
              -3.08402300119738975254353E+1,
              3.15350626979604161529144E+2,
              -1.01515636749021914166146E+3,
              -3.10777167157231109440444E+3,
              2.25381184209801510330112E+4,
              4.75584627752788110767815E+3,
              -1.34659959864969306392456E+5,
              -1.15132259675553483497211E+5
            );

          val z: Double = y - 1;
//          val num = p.foldLeft(1: Double)({(a,b) => (b+a)*z})
//          val den = q.foldLeft(0: Double)({(a,b) => b*z+a})
            var num: Double = 0.0;
            var den: Double = 1.0;

            for (i <- 0 to 7)
            {
                num = (num + p(i))*z;
                den = den*z + q(i);
            }

          val result = num/den + 1.0;

          // Apply correction if argument was not initially in (1,2)
          if (arg_was_less_than_one)
            {
              // Use identity gamma(z) = gamma(z+1)/z
              // The variable "result" now holds gamma of the original y + 1
              // Thus we use y-1 to get back the orginal y.
              result / (y-1.0);
            }
          else
            {
              // Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
//              List.range(0,n.toInt).map(_.toDouble).foldLeft(result)((a,b) => a*(y+b))
              var hobores = result
              var hoboy = y
              for (i <- 0 to n-1) {
                hobores *= hoboy+1;
                hoboy = hoboy+1
              }
              hobores
            }
        } else if (x <= 171.624) {
          ///////////////////////////////////////////////////////////////////////////
          // Third interval: [12, 171.624)
          hoboTrampoline(x,true,((a: Double) => todo(math.exp(a))));
        } else {
          ///////////////////////////////////////////////////////////////////////////
          // Fourth interval: [171.624, INFINITY)
          // Correct answer too large to display.
          scala.Double.PositiveInfinity
        }
    } else {
      //log implementation
      if (x <= 0.0)
        {
          val msg = "Invalid input argument "+x+". Argument must be positive."
          throw new IllegalArgumentException(msg);
        }

      if (x < 12.0) {
        hoboTrampoline(x,false,((a: Double) => todo(Math.log(Math.abs(a)))));
      } else {

        // Abramowitz and Stegun 6.1.41
        // Asymptotic series should be good to at least 11 or 12 figures
        // For error analysis, see Whittiker and Watson
        // A Course in Modern Analysis (1927), page 252

        val c: Array[Double] =
          Array(
            1.0/12.0,
            -1.0/360.0,
            1.0/1260.0,
            -1.0/1680.0,
            1.0/1188.0,
            -691.0/360360.0,
            1.0/156.0
          );
        val z: Double = 1.0/(x*x);
        val sum: Double = c.foldRight(-3617.0/122400.0: Double)({(a,b) => b*z+a});
        val series: Double = sum/x;

        val halfLogTwoPi: Double = 0.91893853320467274178032973640562;
        val logGamma: Double = (x - 0.5)*Math.log(x) - x + halfLogTwoPi + series;
        logGamma;
      }
    }
  }


}
object Gamma extends Gamma
