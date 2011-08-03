with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Erf;
with Expm1;
with Phi;
with Normal_CDF_Inverse;
with Gamma;
with Log_Factorial;

procedure test is

    package Test_Erf is new Erf(Long_Float);
    package Test_Expm1 is new Expm1(Long_Float);
    package Test_Phi is new Phi(Long_Float);
    package Test_Normal_CDF_Inverse is new Normal_CDF_Inverse(Long_Float);
    package Test_Gamma is new Gamma(Long_Float);
    package Test_Log_Factorial is new Log_Factorial(Long_Float);

    s: String(1..80);
    i, last: Natural;

begin
    loop
        Ada.Text_IO.Get_Line(s, last);
        i := Ada.Strings.Fixed.Index(s(1..last), " ");
        declare
            f: String := s(1..i-1);
            x: Long_Float := Long_Float'Value(s(i+1..last));
        begin
            if f = "erf" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Erf.Erf(x)));
            elsif f = "expm1" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Expm1.Expm1(x)));
            elsif f = "phi" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Phi.Phi(x)));
            elsif f = "NormalCDFInverse" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Normal_CDF_Inverse.Normal_CDF_Inverse(x)));
            elsif f = "Gamma" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Gamma.Gamma(x)));
            elsif f = "LogGamma" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Gamma.Log_Gamma(x)));
            elsif f = "LogFactorial" then
                Ada.Text_IO.Put_Line(Long_Float'Image(Test_Log_Factorial.Log_Factorial(Natural(x))));
            else
                Ada.Text_IO.Put_Line("Unknown function: " & f);
                exit;
            end if;
        end;
    end loop;
end test;
