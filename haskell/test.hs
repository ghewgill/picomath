import Data.List
import System.IO

import Erf
import Expm1
import Phi
import NormalCdfInverse
import Gamma
import LogFactorial

main = do s <- getLine
          let a = words s
              f = head a
              x = read $ (head . tail) a :: Double
              r = case f of
                      "erf"              -> show $ erf x
                      "expm1"            -> show $ expm1 x
                      "phi"              -> show $ phi x
                      "NormalCDFInverse" -> show $ normalCdfInverse x
                      "Gamma"            -> show $ gamma x
                      "LogGamma"         -> show $ logGamma x
                      "LogFactorial"     -> show $ logFactorial (truncate x)
          putStrLn r
          hFlush stdout
          main
