package normal_cdf_inverse

import "math"

func RationalApproximation (t float64) float64 {
    // Abramowitz and Stegun formula 26.2.23.
    // The absolute value of the error should be less than 4.5 e-4.
    c := []float64{2.515517, 0.802853, 0.010328}
    d := []float64{1.432788, 0.189269, 0.001308}
    numerator := (c[2]*t + c[1])*t + c[0]
    denominator := ((d[2]*t + d[1])*t + d[0])*t + 1.0
    return t - numerator / denominator
}

func NormalCDFInverse (p float64) float64 {

    //assert p > 0.0 and p < 1

    // See article above for explanation of this section.
    if p < 0.5 {
        // F^-1(p) = - G^-1(p)
        return -RationalApproximation( math.Sqrt(-2.0*math.Log(p)) )
    }
    // F^-1(p) = G^-1(1-p)
    return RationalApproximation( math.Sqrt(-2.0*math.Log(1.0-p)) )
}
