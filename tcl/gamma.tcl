# Visit http://www.johndcook.com/stand_alone_code.html for the source of this code and more like it.

# Note that the functions Gamma and LogGamma are mutually dependent.

proc gamma {x} {
    if {$x <= 0} {
        error "Invalid input" $x
    }

    # Split the function domain into three intervals:
    # (0, 0.001), [0.001, 12), and (12, infinity)

    ###########################################################################
    # First interval: (0, 0.001)
    #
    # For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
    # So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
    # The relative error over this interval is less than 6e-7.

    # Euler's gamma constant
    set gamma 0.577215664901532860606512090

    if {$x < 0.001} {
        return [expr 1.0/($x*(1.0 + $gamma*$x))]
    }

    ###########################################################################
    # Second interval: [0.001, 12)

    if {$x < 12.0} {
        # The algorithm directly approximates gamma over (1,2) and uses
        # reduction identities to reduce other arguments to this interval.
        
        set y $x
        set n 0
        set arg_was_less_than_one [expr $y < 1.0]

        # Add or subtract integers as necessary to bring y into (1,2)
        # Will correct for this below
        if {$arg_was_less_than_one} {
            set y [expr $y + 1.0]
        } else {
            # will use n later
            set n [expr entier($y) - 1]
            set y [expr $y - $n]
        }

        # numerator coefficients for approximation over the interval (1,2)
        set p [list \
            -1.71618513886549492533811E+0 \
             2.47656508055759199108314E+1 \
            -3.79804256470945635097577E+2 \
             6.29331155312818442661052E+2 \
             8.66966202790413211295064E+2 \
            -3.14512729688483675254357E+4 \
            -3.61444134186911729807069E+4 \
             6.64561438202405440627855E+4 \
        ]

        # denominator coefficients for approximation over the interval (1,2)
        set q [list \
            -3.08402300119738975254353E+1 \
             3.15350626979604161529144E+2 \
            -1.01515636749021914166146E+3 \
            -3.10777167157231109440444E+3 \
             2.25381184209801510330112E+4 \
             4.75584627752788110767815E+3 \
            -1.34659959864969306392456E+5 \
            -1.15132259675553483497211E+5 \
        ]

        set num 0.0
        set den 1.0

        set z [expr $y - 1]
        for {set i 0} {$i < 8} {incr i} {
            set num [expr ($num + [lindex $p $i])*$z]
            set den [expr $den*$z + [lindex $q $i]]
        }
        set result [expr $num/$den + 1.0]

        # Apply correction if argument was not initially in (1,2)
        if {$arg_was_less_than_one} {
            # Use identity gamma(z) = gamma(z+1)/z
            # The variable "result" now holds gamma of the original y + 1
            # Thus we use y-1 to get back the orginal y.
            set result [expr $result / ($y-1.0)]
        } else {
            # Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
            for {set i 0} {$i < $n} {incr i} {
                set result [expr $result * $y]
                set y [expr $y + 1]
            }
        }

        return $result
    }

    ###########################################################################
    # Third interval: [12, infinity)

    if {$x > 171.624} {
        # Correct answer too large to display. 
        return 1.0/0 # float infinity
    }

    return [expr exp([log_gamma $x])]
}

proc log_gamma {x} {
    if {$x <= 0} {
        error "Invalid input"
    }

    if {$x < 12.0} {
        return [expr log(abs([gamma $x]))]
    }

    # Abramowitz and Stegun 6.1.41
    # Asymptotic series should be good to at least 11 or 12 figures
    # For error analysis, see Whittiker and Watson
    # A Course in Modern Analysis (1927), page 252

    set c [list \
         1.0/12.0 \
        -1.0/360.0 \
         1.0/1260.0 \
        -1.0/1680.0 \
         1.0/1188.0 \
        -691.0/360360.0 \
         1.0/156.0 \
        -3617.0/122400.0 \
    ]
    set z [expr 1.0/($x*$x)]
    set sum [lindex $c 7]
    for {set i 6} {$i >= 0} {incr i -1} {
        set sum [expr $sum * $z]
        set sum [expr $sum + [lindex $c $i]]
    }
    set series [expr $sum/$x]

    set halfLogTwoPi 0.91893853320467274178032973640562
    set logGamma [expr ($x - 0.5)*log($x) - $x + $halfLogTwoPi + $series]
    return $logGamma
}
