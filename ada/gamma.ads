generic
    type Float_Type is digits <>;
package Gamma is

    function Gamma(x: Float_Type'Base) return Float_Type'Base;
    function Log_Gamma(x: Float_Type'Base) return Float_Type'Base;

end Gamma;
