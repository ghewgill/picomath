#include <iomanip>
#include <iostream>

#include "Gamma.h"

extern "C" double erf(double);
extern "C" double expm1(double);
double phi(double);
double NormalCDFInverse(double p);

int main(int, char *[])
{
    std::cout << std::setprecision(17);
    char buf[100];
    while (std::cin.getline(buf, sizeof(buf))) {
        std::string f = strtok(buf, " ");
        const char *a = strtok(NULL, "\n");
        double x = strtod(a, NULL);
        if (f == "erf") {
            std::cout << erf(x) << std::endl;
        } else if (f == "expm1") {
            std::cout << expm1(x) << std::endl;
        } else if (f == "phi") {
            std::cout << phi(x) << std::endl;
        } else if (f == "NormalCDFInverse") {
            std::cout << NormalCDFInverse(x) << std::endl;
        } else if (f == "Gamma") {
            std::cout << Gamma(x) << std::endl;
        } else if (f == "LogGamma") {
            std::cout << LogGamma(x) << std::endl;
        } else {
            std::cerr << "Unknown function: " << f << std::endl;
            break;
        }
    }
}
