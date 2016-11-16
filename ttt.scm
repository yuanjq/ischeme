(define (accumulate op initial seq)
(if (null? seq)
initial
(op (car seq)
(accumulate op initial (cdr seq)))))
(define (enum low high)
(if (> low high)
'()
(cons low (enum (+ low 1) high))))
(define (filter predicate seq)
  (cond ((null? seq) '()) 
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))
(define (safe? new-col old-cols)
(if (null? old-cols)
 #t
(and (not (= new-col (car old-cols)))
(not (= (car old-cols) (- new-col (length old-cols))))
(not (= (car old-cols) (+ new-col (length old-cols))))
(safe? new-col (cdr old-cols)))))
(define (insert-tail new-col old-cols)
(if (null? old-cols)
(list new-col)
(cons (car old-cols) (insert-tail new-col (cdr old-cols)))))
(define (quenes size)
(define (quenes-iter k)
  (if (= k 0) 
    (list '())
    (accumulate append '()
                (map (lambda (old-cols)
                       (map (lambda (new-col)
                              (insert-tail new-col old-cols))
                            (filter (lambda (new-col)
                                      (safe? new-col old-cols))
                                    (enum 1 size))))
                     (quenes-iter (- k 1))))))
(quenes-iter size))

(display (quenes 8))
