source "erf.tcl"
source "expm1.tcl"
source "phi.tcl"
source "normal_cdf_inverse.tcl"
source "gamma.tcl"
source "log_factorial.tcl"

while {[gets stdin s] >= 0} {
    set a [split $s]
    set f [lindex $a 0]
    set x [lindex $a 1]
    if {[string equal $f "erf"]} {
        puts [erf $x]
    } elseif {[string equal $f "expm1"]} {
        puts [expm1 $x]
    } elseif {[string equal $f "phi"]} {
        puts [phi $x]
    } elseif {[string equal $f "NormalCDFInverse"]} {
        puts [normal_cdf_inverse $x]
    } elseif {[string equal $f "Gamma"]} {
        puts [gamma $x]
    } elseif {[string equal $f "LogGamma"]} {
        puts [log_gamma $x]
    } elseif {[string equal $f "LogFactorial"]} {
        puts [log_factorial [expr entier($x)]]
    } else {
        puts "Unknown function: $f"
        break
    }
}
