(define (sieve n l x)
	(display (length l))
	(newline)
	(cond
		((eq? (length l) n) l)
		((prime? x l) (sieve n (cons x l) (+ 1 x)))
		(else (sieve n l (+ 1 x)))))

(define (prime? x l)
	(cond
		((null? l) #t)
		((eq? (modulo x (car l)) 0) #f)
		(else (prime? x (cdr l)))))

(define (get-first-n-primes n)
	(sieve n '() 2))


104743

