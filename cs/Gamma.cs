using System;

namespace ConsoleApplication1
{
    class Program
    {
        // Visit http://www.johndcook.com/stand_alone_code.html for the source of this code and more like it.

        // Note that the functions Gamma and LogGamma are mutually dependent.

        public static double Gamma
        (
            double x    // We require x > 0
        )
        {
            if (x <= 0.0)
            {
                string msg = string.Format("Invalid input argument {0}. Argument must be positive.", x);
                throw new ArgumentOutOfRangeException(msg);
            }

            // Split the function domain into three intervals:
            // (0, 0.001), [0.001, 12), and (12, infinity)

            ///////////////////////////////////////////////////////////////////////////
            // First interval: (0, 0.001)
	        //
	        // For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
	        // So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
	        // The relative error over this interval is less than 6e-7.

	        const double gamma = 0.577215664901532860606512090; // Euler's gamma constant

            if (x < 0.001)
                return 1.0/(x*(1.0 + gamma*x));

            ///////////////////////////////////////////////////////////////////////////
            // Second interval: [0.001, 12)
    
	        if (x < 12.0)
            {
                // The algorithm directly approximates gamma over (1,2) and uses
                // reduction identities to reduce other arguments to this interval.
		
		        double y = x;
                int n = 0;
                bool arg_was_less_than_one = (y < 1.0);

                // Add or subtract integers as necessary to bring y into (1,2)
                // Will correct for this below
                if (arg_was_less_than_one)
                {
                    y += 1.0;
                }
                else
                {
                    n = (int) (Math.Floor(y)) - 1;  // will use n later
                    y -= n;
                }

                // numerator coefficients for approximation over the interval (1,2)
                double[] p =
                {
                    -1.71618513886549492533811E+0,
                     2.47656508055759199108314E+1,
                    -3.79804256470945635097577E+2,
                     6.29331155312818442661052E+2,
                     8.66966202790413211295064E+2,
                    -3.14512729688483675254357E+4,
                    -3.61444134186911729807069E+4,
                     6.64561438202405440627855E+4
                };

                // denominator coefficients for approximation over the interval (1,2)
                double[] q =
                {
                    -3.08402300119738975254353E+1,
                     3.15350626979604161529144E+2,
                    -1.01515636749021914166146E+3,
                    -3.10777167157231109440444E+3,
                     2.25381184209801510330112E+4,
                     4.75584627752788110767815E+3,
                    -1.34659959864969306392456E+5,
                    -1.15132259675553483497211E+5
                };

                double num = 0.0;
                double den = 1.0;
                int i;

                double z = y - 1;
                for (i = 0; i < 8; i++)
                {
                    num = (num + p[i])*z;
                    den = den*z + q[i];
                }
                double result = num/den + 1.0;

                // Apply correction if argument was not initially in (1,2)
                if (arg_was_less_than_one)
                {
                    // Use identity gamma(z) = gamma(z+1)/z
                    // The variable "result" now holds gamma of the original y + 1
                    // Thus we use y-1 to get back the orginal y.
                    result /= (y-1.0);
                }
                else
                {
                    // Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
                    for (i = 0; i < n; i++)
                        result *= y++;
                }

		        return result;
            }

            ///////////////////////////////////////////////////////////////////////////
            // Third interval: [12, infinity)

            if (x > 171.624)
            {
		        // Correct answer too large to display. 
		        return double.PositiveInfinity;
            }

            return Math.Exp(LogGamma(x));
        }

        public static double LogGamma
        (
            double x    // x must be positive
        )
        {
	        if (x <= 0.0)
	        {
		        string msg = string.Format("Invalid input argument {0}. Argument must be positive.", x);
                throw new ArgumentOutOfRangeException(msg);
	        }

            if (x < 12.0)
            {
                return Math.Log(Math.Abs(Gamma(x)));
            }

	        // Abramowitz and Stegun 6.1.41
            // Asymptotic series should be good to at least 11 or 12 figures
            // For error analysis, see Whittiker and Watson
            // A Course in Modern Analysis (1927), page 252

            double[] c =
            {
		         1.0/12.0,
		        -1.0/360.0,
		        1.0/1260.0,
		        -1.0/1680.0,
		        1.0/1188.0,
		        -691.0/360360.0,
		        1.0/156.0,
		        -3617.0/122400.0
            };
            double z = 1.0/(x*x);
            double sum = c[7];
            for (int i=6; i >= 0; i--)
            {
                sum *= z;
                sum += c[i];
            }
            double series = sum/x;

            double halfLogTwoPi = 0.91893853320467274178032973640562;
            double logGamma = (x - 0.5)*Math.Log(x) - x + halfLogTwoPi + series;    
	        return logGamma;
        }

        public static void TestGamma()
        {
            Console.WriteLine("Testing Gamma()");

	        double[,] test = 
	        {
		        // Test near branches in code for (0, 0.001), [0.001, 12), (12, infinity)
		        {1e-20, 1e+20},
		        {2.19824158876e-16, 4.5490905327e+15},	
		        {2.24265050974e-16, 4.45900953205e+15},
		        {0.00099, 1009.52477271},
		        {0.00100, 999.423772485},
		        {0.00101, 989.522792258},
		        {6.1, 142.451944066},
		        {11.999, 39819417.4793},
		        {12, 39916800.0},
		        {12.001, 40014424.1571},
		        {15.2, 149037380723.0}
	        };

	        double worst_absolute_error = 0.0;
	        double worst_relative_error = 0.0;
	        int worst_absolute_error_case = 0;
	        int worst_relative_error_case = 0;

            int t;

	        for (t = 0; t < test.GetUpperBound(0); t++)
	        {
		        double computed = Gamma( test[t, 0] );
		        double absolute_error = Math.Abs(computed - test[t, 1]);
		        double relative_error = absolute_error / test[t, 1];

		        if (absolute_error > worst_absolute_error)
		        {
			        worst_absolute_error = absolute_error;
			        worst_absolute_error_case = t;
		        }

		        if (relative_error > worst_relative_error)
		        {
			        worst_relative_error = absolute_error;
			        worst_relative_error_case = t;
		        }
	        }

	        t = worst_absolute_error_case;
	        double x = test[t, 0];
	        double y = test[t, 1];
            Console.WriteLine("Worst absolute error: {0}. Gamma({1}) computed as {2} but exact value is {3}", 
			          Math.Abs(Gamma(x) - y), x, Gamma(x), y);
	
	        t = worst_relative_error_case;
	        x = test[t, 0];
	        y = test[t, 1];
            Console.WriteLine("Worst relative error: {0}. Gamma({1}) computed as {2} but exact value is {3}",
                      Math.Abs(Gamma(x) - y) / y, x, Gamma(x), y);
        }

        public static void TestLogGamma()
        {
            Console.WriteLine("Testing LogGamma()"); 
            
            double[,] test = 
	        {
		        {1e-12, 27.6310211159},
		        {0.9999, 5.77297915613e-05},
		        {1.0001, -5.77133422205e-05},
		        {3.1, 0.787375083274},
		        {6.3, 5.30734288962},
		        {11.9999, 17.5020635801},
		        {12, 17.5023078459},
		        {12.0001, 17.5025521125},
		        {27.4, 62.5755868211}
	        };

	        double worst_absolute_error = 0.0;
	        double worst_relative_error = 0.0;
	        int worst_absolute_error_case = 0;
	        int worst_relative_error_case = 0;

            int t;

            for (t = 0; t < test.GetUpperBound(0); t++)
	        {
		        double computed = LogGamma( test[t, 0] );
		        double absolute_error = Math.Abs(computed - test[t, 1]);
		        double relative_error = absolute_error / test[t, 1];

		        if (absolute_error > worst_absolute_error)
		        {
			        worst_absolute_error = absolute_error;
			        worst_absolute_error_case = t;
		        }

		        if (relative_error > worst_relative_error)
		        {
			        worst_relative_error = absolute_error;
			        worst_relative_error_case = t;
		        }
	        }

	        t = worst_absolute_error_case;
	        double x = test[t, 0];
	        double y = test[t, 1];
            Console.WriteLine("Worst absolute error: {0}. LogGamma({1}) computed as {2} but exact value is {3}", 
			          Math.Abs(LogGamma(x) - y), x, LogGamma(x), y);

	        t = worst_relative_error_case;
	        x = test[t, 0];
	        y = test[t, 1];
            Console.WriteLine("Worst relative error: {0}. LogGamma({1}) computed as {2} but exact value is {3}",
                      Math.Abs(LogGamma(x) - y)/y, x, LogGamma(x), y);

        }

        static void Main(string[] args)
        {
            TestGamma();
            TestLogGamma();
        }
    }
}
