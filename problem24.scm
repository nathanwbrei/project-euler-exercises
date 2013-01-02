(define (permutation-stream current alphabet)
    (let ((next 22))
        (cons-stream next (permutation-stream next alphabet))))

(define (run)
    (stream-ref (permutation-stream '(0 1 2 3 4 5 6 7 8 9) '(0 1 2 3 4 5 6 7 8 9)) 1000000))


(define (rember l a)
    (cond 
        ((null? l) '())
        ((eq? (car l) a) (rember (cdr l) a))
        (else (cons (car l) (rember (cdr l) a)))))

(define (rembers set subset)
    (if (null? subset)
        set
        (rembers (rember set (car subset)) (cdr subset)))))

(define (smallest l)
    (define (smallest l a)
        (cond
            ((null? l) a)
            ((< (car l) a) (smallest (cdr l) (car l)))
            (else (smallest (cdr l) a))))
    (smallest l (car l)))

(define (failpick n unused)
    (if (eq? n 0)
        unused
        (let ((x (smallest unused)))
            (cons x (pick (- 1 n) (rember unused x))))))

(define (pick n alphabet unused)
    (cond
        ((null? alphabet) '())
        (else (cons (cons (car alphabet) (pick (- 1 n) alphabet (rember unused (car alphabet)) backupalphabet))
                    (pick n (cdr alphabet) backupalphabet backupalphabet)))))

 