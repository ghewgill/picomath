sub phi {
    my $x = $_[0];

    # constants
    my $a1 =  0.254829592;
    my $a2 = -0.284496736;
    my $a3 =  1.421413741;
    my $a4 = -1.453152027;
    my $a5 =  1.061405429;
    my $p  =  0.3275911;

    # Save the sign of x
    my $sign = 1;
    if ($x < 0) {
        $sign = -1;
    }
    $x = abs($x)/sqrt(2.0);

    # A&S formula 7.1.26
    my $t = 1.0/(1.0 + $p*$x);
    my $y = 1.0 - ((((($a5*$t + $a4)*$t) + $a3)*$t + $a2)*$t + $a1)*$t*exp(-$x*$x);

    return 0.5*(1.0 + $sign*$y);
}

1;
