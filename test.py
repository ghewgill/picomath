import subprocess
import os
import sys

Languages = {
    "cpp":  "./test",
    "cs":   ["mono", "test.exe"],
    "erl":  ["escript", "test.erl"],
    "java": ["java", "Test"],
    "js":   ["java", "-cp", "js.jar", "org.mozilla.javascript.tools.shell.Main", "test.js"],
    "php":  ["php", "test.php"],
    "pl":   ["perl", "test.pl"],
    "py":   [sys.executable, "test.py"],
    "rb":   ["ruby", "test.rb"],
}

class Driver:
    def __init__(self, lang):
        self.lang = lang
        self.pipe = subprocess.Popen(Languages[lang], stdin=subprocess.PIPE, stdout=subprocess.PIPE, cwd=lang)
    def call(self, name, x):
        self.pipe.stdin.write(("%s %e\n" % (name, x)).encode())
        self.pipe.stdin.flush()
        s = self.pipe.stdout.readline()
        return float(s)
    def close(self):
        self.pipe.stdin.close()
        self.pipe.wait()

def test_erf(driver, log):
    Tests = (
        (-3,  -0.999977909503),
        (-1,  -0.842700792950),
        (0.0, 0.0),
        (0.5, 0.520499877813),
        (2.1, 0.997020533344),
    )
    maxError = 0
    for x, y in Tests:
        error = abs(y - driver.call("erf", x))
        if error > maxError:
            maxError = error
    log.append("erf: Maximum error: " + str(maxError))
    return maxError < 1e-6

def test_expm1(driver, log):
    Tests = (
        (-1,          -0.632120558828558),
        (0.0,          0.0),
        (1e-5 - 1e-8,  0.000009990049900216168),
        (1e-5 + 1e-8,  0.00001001005010021717),
        (0.5,          0.6487212707001282),
    )
    maxError = 0
    for x, y in Tests:
        error = abs(y - driver.call("expm1", x))
        if error > maxError:
            maxError = error
    log.append("expm1: Maximum error: " + str(maxError))
    return maxError < 1e-6

def test_phi(driver, log):
    Tests = (
        (-3,  0.00134989803163),
        (-1,  0.158655253931),
        (0.0, 0.5),
        (0.5, 0.691462461274),
        (2.1, 0.982135579437),
    )
    maxError = 0
    for x, y in Tests:
        error = abs(y - driver.call("phi", x))
        if error > maxError:
            maxError = error
    log.append("phi: Maximum error: " + str(maxError))
    return maxError < 1e-6

def test_NormalCDFInverse(driver, log):
    Tests = (
        (0.0000001, -5.199337582187471),
        (0.00001,   -4.264890793922602),
        (0.001,     -3.090232306167813),
        (0.05,      -1.6448536269514729),
        (0.15,      -1.0364333894937896),
        (0.25,      -0.6744897501960817),
        (0.35,      -0.38532046640756773),
        (0.45,      -0.12566134685507402),
        (0.55,       0.12566134685507402),
        (0.65,       0.38532046640756773),
        (0.75,       0.6744897501960817),
        (0.85,       1.0364333894937896),
        (0.95,       1.6448536269514729),
        (0.999,      3.090232306167813),
        (0.99999,    4.264890793922602),
        (0.9999999,  5.199337582187471),
    )
    maxError = 0
    for x, y in Tests:
        error = abs(y - driver.call("NormalCDFInverse", x))
        if error > maxError:
            maxError = error
    log.append("NormalCDFInverse: Maximum error: " + str(maxError))
    return maxError < 1e-3

def test_Gamma(driver, log):
    Tests = (
        (1e-20, 1e+20),
        (2.19824158876e-16, 4.5490905327e+15),    # 0.99*DBL_EPSILON
        (2.24265050974e-16, 4.45900953205e+15),   # 1.01*DBL_EPSILON
        (0.00099, 1009.52477271),
        (0.00100, 999.423772485),
        (0.00101, 989.522792258),
        (6.1, 142.451944066),
        (11.999, 39819417.4793),
        (12, 39916800.0),
        (12.001, 40014424.1571),
        (15.2, 149037380723.0),
    )

    worst_absolute_error = 0.0
    worst_relative_error = 0.0
    worst_absolute_error_case = 0
    worst_relative_error_case = 0

    for i, (x, y) in enumerate(Tests):
        computed = driver.call("Gamma", x)
        absolute_error = abs(computed - y)
        relative_error = absolute_error / y

        if absolute_error > worst_absolute_error:
            worst_absolute_error = absolute_error
            worst_absolute_error_case = i

        if relative_error > worst_relative_error:
            worst_relative_error = relative_error
            worst_relative_error_case = i

    t = worst_absolute_error_case
    x, y = Tests[t]
    a = driver.call("Gamma", x)
    log.append("Gamma: Worst absolute error: " + str(abs(a - y)))
    log.append("Gamma(" + str(x) + ") computed as " + str(a) + " but exact value is " + str(y))

    t = worst_relative_error_case
    x, y = Tests[t]
    a = driver.call("Gamma", x)
    log.append("Gamma: Worst relative error: " + str(abs(a - y) / y))
    log.append("Gamma(" + str(x) + ") computed as " + str(a) + " but exact value is " + str(y))

    return worst_relative_error < 1e-3

def test_LogGamma(driver, log):
    Tests = (
        (1e-12, 27.6310211159),
        (0.9999, 5.77297915613e-05),
        (1.0001, -5.77133422205e-05),
        (3.1, 0.787375083274),
        (6.3, 5.30734288962),
        (11.9999, 17.5020635801),
        (12, 17.5023078459),
        (12.0001, 17.5025521125),
        (27.4, 62.5755868211),
    )

    worst_absolute_error = 0.0
    worst_relative_error = 0.0
    worst_absolute_error_case = 0
    worst_relative_error_case = 0

    for i, (x, y) in enumerate(Tests):
        computed = driver.call("LogGamma", x)
        absolute_error = abs(computed - y)
        relative_error = absolute_error / y

        if absolute_error > worst_absolute_error:
            worst_absolute_error = absolute_error
            worst_absolute_error_case = i

        if relative_error > worst_relative_error:
            worst_relative_error = relative_error
            worst_relative_error_case = i

    t = worst_absolute_error_case
    x, y = Tests[t]
    a = driver.call("LogGamma", x)
    log.append("LogGamma: Worst absolute error: " + str(abs(a - y)))
    log.append("LogGamma(" + str(x) + ") computed as " + str(a) + " but exact value is " + str(y))

    t = worst_relative_error_case
    x, y = Tests[t]
    a = driver.call("LogGamma", x)
    log.append("LogGamma: Worst relative error: " + str(abs(a - y) / y))
    log.append("LogGamma(" + str(x) + ") computed as " + str(a) + " but exact value is " + str(y))

    return worst_relative_error < 1e-2

TestFunctions = (
    test_erf,
    test_expm1,
    test_phi,
    test_NormalCDFInverse,
    test_Gamma,
    test_LogGamma,
)

def tests():
    anyfail = False
    for lang in Languages:
        sys.stdout.write("Checking " + lang + "... ")
        sys.stdout.flush()
        driver = Driver(lang)
        logs = []
        allok = True
        for tf in TestFunctions:
            log = []
            if not tf(driver, log):
                allok = False
                logs.extend(log)
        if allok:
            sys.stdout.write("ok\n")
        else:
            anyfail = True
            sys.stdout.write("FAIL\n")
            for s in logs:
                sys.stdout.write(s + "\n")
        driver.close()
    if anyfail:
        sys.exit(1)

if __name__ == "__main__":
    tests()
