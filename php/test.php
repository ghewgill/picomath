<?

require("erf.php");
#require("expm1.php");
require("phi.php");
require("normal_cdf_inverse.php");
require("gamma.php");

while ($s = fgets(STDIN)) {
    list($f, $x) = explode(" ", $s);
    if ($f == "erf") {
        print erf($x) . "\n";
    } elseif ($f == "expm1") {
        print expm1($x) . "\n";
    } elseif ($f == "phi") {
        print phi($x) . "\n";
    } elseif ($f == "NormalCDFInverse") {
        print normal_cdf_inverse($x) . "\n";
    } elseif ($f == "Gamma") {
        print gamma($x) . "\n";
    } elseif ($f == "LogGamma") {
        print log_gamma($x) . "\n";
    } else {
        fputs(STDERR, "Unknown function: $f\n");
        break;
    }
}

?>
