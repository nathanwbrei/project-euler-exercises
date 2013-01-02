(define (factor bignum fac)
  (cond
	  ((eq? bignum 1) fac)
	  ((eq? (modulo bignum fac) 0) (factor (/ bignum fac) fac))
	  (else (factor bignum (+ 1 fac)))))