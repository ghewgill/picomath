with Ada.Numerics.Generic_Elementary_Functions;

package body Erf is

    package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float_Type'Base);
    use Elementary_Functions;

    function Erf(x: Float_Type'Base) return Float_Type'Base is

        a1: constant :=  0.254829592;
        a2: constant := -0.284496736;
        a3: constant :=  1.421413741;
        a4: constant := -1.453152027;
        a5: constant :=  1.061405429;
        p: constant  :=  0.3275911;

        sign, a, t, y: Float_Type'Base;

    begin
        -- Save the sign of x
        sign := 1.0;
        if x < 0.0 then
            sign := -1.0;
        end if;
        a := Abs(x);

        -- A&S formula 7.1.26
        t := 1.0/(1.0 + p*a);
        y := 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-a*a);

        return sign*y;
    end Erf;

end Erf;
