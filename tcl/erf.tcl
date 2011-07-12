proc erf {x} {
    # constants
    set a1  0.254829592
    set a2 -0.284496736
    set a3  1.421413741
    set a4 -1.453152027
    set a5  1.061405429
    set p   0.3275911

    # Save the sign of x
    set sign 1
    if {$x < 0} {
        set sign -1
    }
    set x [expr abs($x)]

    # A&S formula 7.1.26
    set t [expr 1.0/(1.0 + $p*$x)]
    set y [expr 1.0 - ((((($a5*$t + $a4)*$t) + $a3)*$t + $a2)*$t + $a1)*$t*exp(-$x*$x)]

    return [expr $sign*$y]
}
