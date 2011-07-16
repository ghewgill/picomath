(define (expm1 x)
    (if (< (abs x) 1e-5)
        (+ x (* 0.5 x x))
        (- (exp x) 1.0)))
