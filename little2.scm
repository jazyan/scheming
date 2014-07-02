; chapters 6-10 of The Little Schemer

(define (atom? x) (not (pair? x)))

; CHAPTER 6

; helper, 1st sub exp and 2nd sub exp, doesn't work b/c op not atom
(define sexp1
  (lambda (aexp)
    (car (cdr (aexp)))))

(define sexp2
  (lambda (aexp)
    (car (cdr (cdr (aexp))))))

; contains only numbers and operators, make sure there are spaces
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((equal? (car (cdr aexp)) (quote +))
        (and (numbered? (cdr aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((equal? (car (cdr aexp)) (quote *))
        (and (numbered? (cdr aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((equal? (car (cdr aexp)) (quote expt))
        (and (numbered? (cdr aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (cdr aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; evaluate (1 + 3) as (+ 1 3) 
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((equal? (car (cdr nexp)) (quote +)) 
            (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((equal? (car (cdr nexp)) (quote *))
            (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else
        (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

; new number sys
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define eddn
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (eddn (n (zub1 m))))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; CHAPTER 7

; needed for set?
(define member*
  (lambda (a l)
    (cond 
      ((null? l) (quote #f))
      ((atom? (car l)) (or (equal? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((equal? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member* (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((set? lat) lat)
      ((member* (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else (cons (car lat) 
                  (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else 
        (and (member* (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else 
        (or (member* (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member* (car set1) set2) 
            (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member* (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define setdif
  (lambda (set1 set2)
    (cond 
      ((null? set1) (quote ()))
      ((member* (car set1) set2) (setdif (cdr set1) set2))
      (else (cons (car set1) (setdif (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build 
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define third 
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (first rel))))

(define fullfun?
  (lambda (rel)
    (and (set? (first rel)) (set? (second rel)))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

; CHAPTER 8 lambda/currying/ABSTRACTION

(define rember-f
  (lambda (test? a l)
    (cond 
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (equal? x a))))

(define eq1
  (lambda (x)
    (equal? x 1)))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((equal? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL2 (insert-g seqL))
(define insertR2 (insert-g seqR))
(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember2 
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else
          (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define a-friend
  (lambda (x y)
    (length y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons (car lat) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (car lat) newlat) seen)))

; col = "collector"/ "continuation"
(define multiremberCO
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote ())))
      ((equal? (car lat) a) (multiremberCO a (cdr lat)
        (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else (multiremberCO a (cdr lat)
        (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((equal? (car lat) oldL) 
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((equal? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define help!
  (lambda (lat Lc Rc)
    (and (display lat) (display Lc) (display Rc))))

(define multiinsertLRco
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((equal? (car lat) oldL)
       (multiinsertLRco new oldL oldR (cdr lat) 
        (lambda (newlat L R) 
          (col (cons new (cons oldL newlat)) (+ 1 L) R))))
      ((equal? (car lat) oldR)
       (multiinsertLRco new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons oldR (cons new newlat)) L (+ 1 R)))))
      (else
        (multiinsertLRco new oldL oldR (cdr lat)
         (lambda (newlat L R)
           (col (cons (car lat) newlat) L R)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else
        (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define help-eoCO*!
  (lambda (L E O)
    (and (display L) (display E) (display O))))

(define test-eoCO*! '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(define evens-onlyCO*
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) 
          (evens-onlyCO* (cdr l) 
            (lambda (newl E O) 
              (col (cons (car l) newl) (* (car l) E) O))))
         (else
           (evens-onlyCO* (cdr l)
            (lambda (newl E O)
              (col newl E (+ (car l) O)))))))
      (else
        (evens-onlyCO* (car l) (lambda (al ae ao) 
           (evens-onlyCO* (cdr l) (lambda (bl be bo)
              (col (cons al bl) (* ae be) (+ ao bo))))))))))

; CHAPTER 9: partial functions

(define eternity
  (lambda (x)
    (eternity x)))

; takes ((a b) c) to (a (b c))
(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

; total
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair (first pora))
       (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else 
        (+ (* (weight* (first pora)) 2)
           (weight* (second pora)))))))

; partial: ((a b) (c d)) to ((c d) (d c)) to ((a b) (c d)) ...
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair (first pora)) (shuffle (revapir pora)))
      (else (build (first pora) (shuffle (second pora)))))))

; no one knows if this is partial or total??
(define C
  (lambda (n)
    (cond
      ((= 1 n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ 1 (* 3 n)))))))

(define A
  (lambda (n m)
    (cond 
      ((zero? n) (+ 1 m))
      ((zero? m) (A (- 1 n) 1))
      (else (A (- 1 n) (A n (- 1 m)))))))

; cannot define in language! Turing and Godel
(define will-stop?
  (lambda (x)
    (cond
      (will-stop? x #t)
      (else #f))))

; if last-try stops, then (and #t #f) -> #f, but if last-try doesn't
; then (and #f ...) -> which stops -> #t
(define last-try
  (lambda (x)
    (and (will-stop? last-try) (eternity x))))

; length
(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))

; w/o define, same as len_0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

; len_1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (+ 1 
           ((lambda (l) 
            (cond
              ((null? l) 0)
              (else (+ 1 (eternity (cdr l)))))) (cdr l))))))

; len_0
((lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))) eternity)

; len_1
((lambda (len)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (+ 1 (len (cdr l)))))))
 ((lambda (len2)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len2 (cdr l)))))))
  eternity))

; len_0
((lambda (mk-len)
   (mk-len eternity))
 (lambda (len)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; len_0
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (len)
   (lambda (l)
     (cond 
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; what is going on??
((lambda (mk-len)
   (mk-len mk-len))
 (lambda (mk-len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 ((mk-len eternity) (cdr l))))))))

; almighty Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
