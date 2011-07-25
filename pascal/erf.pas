unit erf;

interface

uses math;

function erf(x: float): float;

implementation

function erf(x: float): float;

{ constants }
const
    a1 =  0.254829592;
    a2 = -0.284496736;
    a3 =  1.421413741;
    a4 = -1.453152027;
    a5 =  1.061405429;
    p  =  0.3275911;

var sign: float;
    t, y: float;

begin
    { Save the sign of x }
    sign := 1;
    if x < 0 then
        sign := -1;
    x := abs(x);

    { A&S formula 7.1.26 }
    t := 1.0/(1.0 + p*x);
    y := 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);

    erf := sign*y;
end;

end.
