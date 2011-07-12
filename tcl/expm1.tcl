proc expm1 {x} {
    if {[expr abs($x) < 1e-5]} {
        return [expr $x + 0.5*$x*$x]
    } else {
        return [expr exp($x) - 1.0]
    }
}
