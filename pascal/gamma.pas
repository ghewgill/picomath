unit gamma;

interface

uses math;

function gamma(x: float): float;
function log_gamma(x: float): float;

implementation

{ Visit http://www.johndcook.com/stand_alone_code.html for the source of this code and more like it. }

{ Note that the functions Gamma and LogGamma are mutually dependent. }

function gamma(x: float): float;

const
    euler_gamma = 0.577215664901532860606512090; { Euler's gamma constant }

    { numerator coefficients for approximation over the interval (1,2) }
    p: array [0..7] of float = (
        -1.71618513886549492533811E+0,
         2.47656508055759199108314E+1,
        -3.79804256470945635097577E+2,
         6.29331155312818442661052E+2,
         8.66966202790413211295064E+2,
        -3.14512729688483675254357E+4,
        -3.61444134186911729807069E+4,
         6.64561438202405440627855E+4
    );

    { denominator coefficients for approximation over the interval (1,2) }
    q: array [0..7] of float = (
        -3.08402300119738975254353E+1,
         3.15350626979604161529144E+2,
        -1.01515636749021914166146E+3,
        -3.10777167157231109440444E+3,
         2.25381184209801510330112E+4,
         4.75584627752788110767815E+3,
        -1.34659959864969306392456E+5,
        -1.15132259675553483497211E+5
    );

var y: float;
    n: integer;
    arg_was_less_than_one: boolean;
    num, den: float;
    z: float;
    i: integer;
    result: float;

begin
    Assert(x > 0);

    { Split the function domain into three intervals: }
    { (0, 0.001), [0.001, 12), and (12, infinity) }

    (**************************************************************************
      First interval: (0, 0.001)
     
      For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
      So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
      The relative error over this interval is less than 6e-7.
    *)


    if x < 0.001 then
        gamma := 1.0/(x*(1.0 + euler_gamma*x))

    (**************************************************************************
      Second interval: [0.001, 12)
    *)

    else if x < 12.0 then begin
        { The algorithm directly approximates gamma over (1,2) and uses }
        { reduction identities to reduce other arguments to this interval. }
        
        y := x;
        n := 0;
        arg_was_less_than_one := (y < 1.0);

        { Add or subtract integers as necessary to bring y into (1,2) }
        { Will correct for this below }
        if arg_was_less_than_one then
            y := y + 1.0
        else begin
            n := integer(floor(y)) - 1;  { will use n later }
            y := y - n;
        end;

        num := 0.0;
        den := 1.0;

        z := y - 1;
        for i := 0 to 7 do begin
            num := (num + p[i])*z;
            den := den*z + q[i];
        end;
        result := num/den + 1.0;

        { Apply correction if argument was not initially in (1,2) }
        if arg_was_less_than_one then
            { Use identity gamma(z) = gamma(z+1)/z }
            { The variable "result" now holds gamma of the original y + 1 }
            { Thus we use y-1 to get back the orginal y. }
            result := result / (y-1.0)
        else
            { Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z) }
            for i := 1 to n do begin
                result := result * y;
                y := y + 1;
            end;

        gamma := result;
    end

    (**************************************************************************
      Third interval: [12, infinity)
    *)

    else if x > 171.624 then
        { Correct answer too large to display. }
        gamma := 1.0/0 { float infinity }
    else
        gamma := exp(log_gamma(x))

end;

function log_gamma(x: float): float;

const
    c: array [0..7] of float = (
         1.0/12.0,
        -1.0/360.0,
         1.0/1260.0,
        -1.0/1680.0,
         1.0/1188.0,
        -691.0/360360.0,
         1.0/156.0,
        -3617.0/122400.0
    );
    halfLogTwoPi = 0.91893853320467274178032973640562;

var z: float;
    sum: float;
    i: integer;
    series: float;

begin
    Assert(x > 0);

    if x < 12.0 then
        log_gamma := ln(abs(gamma(x)))
    else begin

        { Abramowitz and Stegun 6.1.41 }
        { Asymptotic series should be good to at least 11 or 12 figures }
        { For error analysis, see Whittiker and Watson }
        { A Course in Modern Analysis (1927), page 252 }

        z := 1.0/(x*x);
        sum := c[7];
        for i := 6 downto 0 do begin
            sum := sum * z;
            sum := sum + c[i];
        end;
        series := sum/x;

        log_gamma := (x - 0.5)*ln(x) - x + halfLogTwoPi + series;
    end;
end;

end.
