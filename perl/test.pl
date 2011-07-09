#!/usr/bin/perl -w

use strict;

require "erf.pl";
require "expm1.pl";
require "phi.pl";
require "normal_cdf_inverse.pl";
require "gamma.pl";

$|++;
while (<>) {
    my ($f, $x) = split;
    if ($f eq "erf") {
        print erf($x), "\n";
    } elsif ($f eq "expm1") {
        print expm1($x), "\n";
    } elsif ($f eq "phi") {
        print phi($x), "\n";
    } elsif ($f eq "NormalCDFInverse") {
        print normal_cdf_inverse($x), "\n";
    } elsif ($f eq "Gamma") {
        print gamma($x), "\n";
    } elsif ($f eq "LogGamma") {
        print log_gamma($x), "\n";
    } else {
        print STDERR "Unknown function: $f\n";
        last;
    }
}
