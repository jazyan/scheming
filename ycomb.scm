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


; This is super repetitive, though. To see that, makes all of lenx -> len
((lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l)))))))
 ((lambda (len)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len (cdr l)))))))
  ((lambda (len)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (len (cdr l)))))))
   ???)))


; Let's abstract. Define "mk-len" that takes in len, then applies len on ???
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


; Above only works for len = 0. Last call of mk-len has no value for len.
; Let's change len to mk-len! It doesn't change the function.
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-len (cdr l))))))))


; How can we make the above work for len <= 1?
; Sub in len for mk-len in the inner lambda if you are confused.
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-len ??? (cdr l))))))))


; Apply the same trick. We don't want inf loop, we want inf loop of mk-len
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-len mk-len (cdr l))))))))

; This works by calling len until we reach the null case!


; To get the YComb, we can pull out the latter "mk-len mk-len"
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   ((lambda (len)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (+ 1 (len (cdr l)))))))
    (mk-len mk-len))))


; But the above is wrong. See http://stackoverflow.com/questions/10499514/y-combinator-discussion-in-the-little-schemer for why.
; The below is correct b/c it has applicative order of evaluation
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   ((lambda (len)
      (lambda (l)
        (cond 
          ((null? l) 0)
          (else (+ 1 (len (cdr l)))))))
    (lambda (x) ((mk-len mk-len) x)))))


; We can next extract out the function len, b/c it does not dep on mk-len
((lambda (le)
   ((lambda (mk-len)
      (mk-len mk-len))
    (lambda (mk-len)
      (le (lambda (x) ((mk-len mk-len) x))))))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))


; Take the top part -- it's our Y comb!
(lambda (le)
  ((lambda (mk-len)
     (mk-len mk-len))
   (lambda (mk-len)
     (le (lambda (x) ((mk-len mk-len) x))))))

(define Y
  (lambda (g)
    ((lambda (f) (f f))
     (lambda (f)
       (g (lambda (x) ((f f) x)))))))


; Can you figure out the below?
(Y Y)

; Me neither.
