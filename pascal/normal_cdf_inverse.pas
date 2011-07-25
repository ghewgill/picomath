unit normal_cdf_inverse;

interface

uses math;

function normal_cdf_inverse(p: float): float;

implementation

function rational_approximation(t: float): float;

const
    c: array [0..2] of float = (2.515517, 0.802853, 0.010328);
    d: array [0..2] of float = (1.432788, 0.189269, 0.001308);

var numerator: float;
    denominator: float;

begin
    { Abramowitz and Stegun formula 26.2.23. }
    { The absolute value of the error should be less than 4.5 e-4. }
    numerator := (c[2]*t + c[1])*t + c[0];
    denominator := ((d[2]*t + d[1])*t + d[0])*t + 1.0;
    rational_approximation := t - numerator / denominator;
end;

function normal_cdf_inverse(p: float): float;

begin
    assert((p > 0.0) and (p < 1));

    { See article above for explanation of this section. }
    if p < 0.5 then
        { F^-1(p) = - G^-1(p) }
        normal_cdf_inverse := -rational_approximation( sqrt(-2.0*ln(p)) )
    else
        { F^-1(p) = G^-1(1-p) }
        normal_cdf_inverse := rational_approximation( sqrt(-2.0*ln(1.0-p)) );
end;

end.
