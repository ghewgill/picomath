unit expm1;

interface

uses math;

function expm1(x: float): float;

implementation

function expm1(x: float): float;

begin
    if abs(x) < 1e-5 then
        expm1 := x + 0.5*x*x
    else
        expm1 := exp(x) - 1.0;
end;

end.
