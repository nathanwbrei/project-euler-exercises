#!/usr/local/bin/mit-scheme


;; Calculates the sum of squares minus the square of sums.

;; there has got to be a linspace command somewhere, dammit
(define (s-to-n s n)
  (cond
    ((> s n) '())	
    (else (cons s (s-to-n (+ 1 s) n)))))

(define (sumsqr n)
  (reduce + 0 (map square (s-to-n 1 n))))

(define (sqrsum n)
  (square (* (/ n 2) (+ n 1))))

(- (sqrsum 100) (sumsqr 100))