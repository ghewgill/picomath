program test;

uses math,
     strutils,
     sysutils,

     erf,
     expm1,
     phi,
     normal_cdf_inverse,
     gamma,
     log_factorial;

var s: string;
    f: string;
    x: float;

begin
    while true do begin
        ReadLn(s);
        f := ExtractWord(1, s, [' ']);
        x := StrToFloat(ExtractWord(2, s, [' ']));
        if f = 'erf' then
            WriteLn(erf.erf(x))
        else if f = 'expm1' then
            WriteLn(expm1.expm1(x))
        else if f = 'phi' then
            WriteLn(phi.phi(x))
        else if f = 'NormalCDFInverse' then
            WriteLn(normal_cdf_inverse.normal_cdf_inverse(x))
        else if f = 'Gamma' then
            WriteLn(gamma.gamma(x))
        else if f = 'LogGamma' then
            WriteLn(gamma.log_gamma(x))
        else if f = 'LogFactorial' then
            WriteLn(log_factorial.log_factorial(floor(x)))
        else begin
            WriteLn('Unknown function: ', f);
            exit;
        end;
        Flush(Output);
    end;
end.
