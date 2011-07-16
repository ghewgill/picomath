; Visit http://www.johndcook.com/stand_alone_code.html for the source of this code and more like it.

; Note that the functions Gamma and LogGamma are mutually dependent.

(define (gamma x)

    (if (<= x 0)
        (error "Invalid input"))

    ; Split the function domain into three intervals:
    ; (0, 0.001), [0.001, 12), and (12, infinity)

    (cond

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; First interval: (0, 0.001)
        ;
        ; For small x, 1/Gamma(x) has power series x + gamma x^2  - ...
        ; So in this range, 1/Gamma(x) = x + gamma x^2 with error on the order of x^3.
        ; The relative error over this interval is less than 6e-7.

        ((< x 0.001)
            (let ((gamma 0.577215664901532860606512090)) ; Euler's gamma constant
                (/ 1.0 (* x (+ 1.0 (* gamma x))))))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; Second interval: [0.001, 12)

        ((< x 12.0)
            ; The algorithm directly approximates gamma over (1,2) and uses
            ; reduction identities to reduce other arguments to this interval.

            (letrec ((gamma-iter (lambda (z num den ps qs)
                         (if (null? ps)
                             (list num den)
                             (let ((new-num (* (+ num (car ps)) z))
                                   (new-den (+ (* den z) (car qs))))
                                 (gamma-iter z new-num new-den (cdr ps) (cdr qs))))))
                     (gamma-z-n (lambda (result y n)
                         (if (= n 0)
                             result
                             (gamma-z-n (* result y) (+ y 1) (- n 1))))))

                (let* ((arg-was-less-than-one (< x 1.0))
                       ; Add or subtract integers as necessary to bring y into (1,2)
                       ; Will correct for this below
                       (n (if arg-was-less-than-one 0 (- (truncate x) 1)))
                       (y (if arg-was-less-than-one (+ x 1.0) (- x n)))

                       ; numerator coefficients for approximation over the interval (1,2)
                       (p '(-1.71618513886549492533811E+0
                             2.47656508055759199108314E+1
                            -3.79804256470945635097577E+2
                             6.29331155312818442661052E+2
                             8.66966202790413211295064E+2
                            -3.14512729688483675254357E+4
                            -3.61444134186911729807069E+4
                             6.64561438202405440627855E+4))

                       ; denominator coefficients for approximation over the interval (1,2)
                       (q '(-3.08402300119738975254353E+1
                             3.15350626979604161529144E+2
                            -1.01515636749021914166146E+3
                            -3.10777167157231109440444E+3
                             2.25381184209801510330112E+4
                             4.75584627752788110767815E+3
                            -1.34659959864969306392456E+5
                            -1.15132259675553483497211E+5))

                       (z (- y 1))
                       (a (gamma-iter z 0.0 1.0 p q))
                       (num (car a))
                       (den (cadr a))
                       (result (+ (/ num den) 1.0)))

                    ; Apply correction if argument was not initially in (1,2)
                    (if arg-was-less-than-one
                        (/ result (- y 1.0))
                        (gamma-z-n result y n)))))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ; Third interval: [12, infinity)

            ((<= x 171.624)
                (exp (log-gamma x)))

            (else
                ; Correct answer too large to display. 
                (inf))))

(define (log-gamma x)
    (if (<= x 0)
        (error "Invalid input"))

    (if (< x 12.0)
        (log (abs (gamma x)))

        ; Abramowitz and Stegun 6.1.41
        ; Asymptotic series should be good to at least 11 or 12 figures
        ; For error analysis, see Whittiker and Watson
        ; A Course in Modern Analysis (1927), page 252

        (letrec ((log-gamma-iter (lambda (z sum c)
                     (if (null? c)
                         sum
                         (let ((s (+ (* sum z) (car c))))
                             (log-gamma-iter z s (cdr c)))))))

            (let* ((c (reverse (list (/  1.0 12.0)
                                     (/ -1.0 360.0)
                                     (/  1.0 1260.0)
                                     (/ -1.0 1680.0)
                                     (/  1.0 1188.0)
                                     (/ -691.0 360360.0)
                                     (/  1.0 156.0)
                                     (/ -3617.0 122400.0))))
                   (z (/ 1.0 (* x x)))
                   (sum (log-gamma-iter z 0 c))
                   (series (/ sum x))
                   (half-log-two-pi 0.91893853320467274178032973640562))
                (+ (* (- x 0.5) (log x)) (- x) half-log-two-pi series)))))
