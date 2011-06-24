sub rational_approximation {
    my $t = $_[0];

    # Abramowitz and Stegun formula 26.2.23.
    # The absolute value of the error should be less than 4.5 e-4.
    my @c = (2.515517, 0.802853, 0.010328);
    my @d = (1.432788, 0.189269, 0.001308);
    my $numerator = ($c[2]*$t + $c[1])*$t + $c[0];
    my $denominator = (($d[2]*$t + $d[1])*$t + $d[0])*$t + 1.0;
    return $t - $numerator / $denominator;
}


sub normal_cdf_inverse {
    my $p = $_[0];

    # See article above for explanation of this section.
    if ($p < 0.5) {
        # F^-1(p) = - G^-1(p)
        return -rational_approximation( sqrt(-2.0*log($p)) );
    } else {
        # F^-1(p) = G^-1(1-p)
        return rational_approximation( sqrt(-2.0*log(1.0-$p)) );
    }
}

1;
