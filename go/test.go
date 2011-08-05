package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "strconv"
    "strings"
)

import (
    "phi"
    "normal_cdf_inverse"
    "log_factorial"
)

func main() {
    for true {
        r := bufio.NewReader(os.Stdin)
        s, err := r.ReadString('\n')
        if err == os.EOF {
            break
        }
        s = strings.TrimRight(s, "\n")
        a := strings.Split(s, " ")
        f := a[0]
        x, err := strconv.Atof64(a[1])
        switch f {
            case "erf":
                fmt.Println(math.Erf(x))
            case "expm1":
                fmt.Println(math.Expm1(x))
            case "phi":
                fmt.Println(phi.Phi(x))
            case "NormalCDFInverse":
                fmt.Println(normal_cdf_inverse.NormalCDFInverse(x))
            case "Gamma":
                fmt.Println(math.Gamma(x))
            case "LogGamma":
                r, _ := math.Lgamma(x)
                fmt.Println(r)
            case "LogFactorial":
                fmt.Println(log_factorial.LogFactorial(int(x)))
            default:
                fmt.Println("Unknown function: " + f)
                return
        }
    }
}
