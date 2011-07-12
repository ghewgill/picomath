proc rational_approximation {t} {
    # Abramowitz and Stegun formula 26.2.23.
    # The absolute value of the error should be less than 4.5 e-4.
    set c [list 2.515517 0.802853 0.010328]
    set d [list 1.432788 0.189269 0.001308]
    set numerator [expr ([lindex $c 2]*$t + [lindex $c 1])*$t + [lindex $c 0]]
    set denominator [expr (([lindex $d 2]*$t + [lindex $d 1])*$t + [lindex $d 0])*$t + 1.0]
    return [expr $t - $numerator / $denominator]
}

proc normal_cdf_inverse {p} {

    #assert p > 0.0 and p < 1

    # See article above for explanation of this section.
    if {$p < 0.5} {
        # F^-1(p) = - G^-1(p)
        return [expr -[rational_approximation [expr sqrt(-2.0*log($p)) ]]]
    } else {
        # F^-1(p) = G^-1(1-p)
        return [rational_approximation [expr sqrt(-2.0*log(1.0-$p)) ]]
    }
}
