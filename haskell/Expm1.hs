module Expm1 (expm1) where

expm1 x = if abs(x) < 1e-5
            then x + 0.5*x*x
            else exp(x) - 1.0
