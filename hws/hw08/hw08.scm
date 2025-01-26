(define (ascending? s) 
    (cond 
    ((null? s) #t)  ;s为空
    ((null? (cdr s)) #t) ;s是单个元素
    ((> (car s) (car (cdr s))) #f) ;条件不符
    (else (ascending? (cdr s)))
    )
)

(define (my-filter pred s) 
    (cond
     ((null? s) '())
     ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
     (else (my-filter pred (cdr s)))
    )
)

(define (interleave lst1 lst2) 
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cons (car lst1) (interleave lst2 (cdr lst1))))
    )
)

(define (no-repeats s) 
    (if (null? s)
        s
        (cons (car s) (no-repeats(filter (lambda (x) (not (= (car s) x))) (cdr s))))
    )
)
