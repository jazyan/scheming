; Derivation of the scary "applicative-order Y combinator"
; at the end of THE LITTLE SCHEMER CH 9

; Eternity. Placeholder for the unknown
(define ???
  (lambda (x)
    (eternity x)))

; Below is length, defined recursively
(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))

; What if you can't use "define"? 
; Since the function has no name, we can't call it recursively. 
; The below only works for null lists.
(lambda (l)
  (cond
    ((null? l) 0)
    (else (+ 1 (??? (cdr l))))))

; The function itself should be where ??? is. 
; But plugging in (lambda (l)...) for ??? only works for lists of len <= 1.
(lambda (l)
  (cond 
    ((null? l) 0)
    (else (+ 1 ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (+ 1 (??? (cdr l))))))
                (cdr l))))))

; Continue this process to make our function work for lists of len <= x. 
; But we want our function to work for all n, not all n <= x.
(lambda (l)
  (cond
    ((null? l) 0)
    (else (+ 1 ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (+ 1 ((lambda (l)
                                  (cond 
                                    ((null? l) 0)
                                    (else (+ 1 (??? (cdr l))))))
                  (cdr l))))))
 (cdr l))))))

; Let's be more abstract. Factor out all of len, or (lambda (l) ...), 
; and put it in an outer lambda. Call it on eternity.
((lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l)))))))
 ???)


; We can do the same thing for len <= 1. Here, ??? gets passed as len2
; in (lambda (len2) ...), which is passed as len1 in (lambda (len1) ...)
((lambda (len0)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len0 (cdr l)))))))
 ((lambda (len1)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len1 (cdr l)))))))
  ???))


; The same goes for len <= 2
((lambda (len0)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len0 (cdr l)))))))
 ((lambda (len1)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len1 (cdr l)))))))
  ((lambda (len2)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (len2 (cdr l)))))))
   ???)))

; We can abstract some more, though! 
; Define a fun "mk-len" which takes in len, then applies len on ???

((lambda (mk-len)
   (mk-len ???))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; Using mk-len, let's define a fun that works for len <= 1
((lambda (mk-len)
   (mk-len mk-len ???))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; We can do the same thing, for len <= 5. You get the point.
((lambda (mk-len)
   (mk-len mk-len mk-len mk-len mk-len ???))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))


; What if we pass in mk-len instead of ??? into mk-len?
; We don't need an inf number of mk-len, but we don't know how many we need.
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))


; The above only works for len = 0. 
; Let's change len to mk-len! It doesn't change the function.
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-len (cdr l))))))))


