(define (normal-cdf-inverse p)
    (let ((rational-approximation
              (lambda (t)
                  ; Abramowitz and Stegun formula 26.2.23.
                  ; The absolute value of the error should be less than 4.5 e-4.
                  (let* ((c #(2.515517 0.802853 0.010328))
                         (d #(1.432788 0.189269 0.001308))
                         (numerator (+ (* (+ (* (vector-ref c 2) t) (vector-ref c 1)) t) (vector-ref c 0)))
                         (denominator (+ (* (+ (* (+ (* (vector-ref d 2) t) (vector-ref d 1)) t) (vector-ref d 0)) t) 1.0)))
                      (- t (/ numerator denominator))))))
        ; assert p > 0.0 and p < 1
        (if (< p 0.5)
            ; F^-1(p) = - G^-1(p)
            (- (rational-approximation (sqrt (* -2.0 (log p)))))
            ; F^-1(p) = G^-1(1-p)
            (rational-approximation (sqrt (* -2.0 (log (- 1.0 p))))))))
