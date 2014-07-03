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

; almighty Y combinator. See ycomb.scm for derivation.
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; CHAPTER 10: interpreter

(define lookup-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((equal? (car names) name) (car vals))
      (else (lookup-help name (cdr names) (cdr vals) entry-f)))))

; entry defines as pair of lists, first list = set, second list same len
(define lookup-entry
  (lambda (name entry entry-f)
    (lookup-help name (first entry) (second entry) entry-f)))

; finds value in list of entries 
(define lookup-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-entry name 
              (car table) (lambda (name)
                            (lookup-table name (cdr table) table-f)))))))

; actions are functions that represent types
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((equal? e #t) *const)
      ((equal? e #f) *const)
      ((equal? e (quote cons)) *const)
      ((equal? e (quote car)) *const)
      ((equal? e (quote cdr)) *const)
      ((equal? e (quote null?)) *const)
      ((equal? e (quote eq?)) *const)
      ((equal? e (quote atom?)) *const)
      ((equal? e (quote zero?)) *const)
      ((equal? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((equal? (car e) (quote quote)) *quote)
         ((equal? (car e) (quote lambda)) *lambda)
         ((equal? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((equal? e #t) #t)
      ((equal? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table 
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

; helpers for evcon
(define else? 
  (lambda (x)
    (cond
      ((atom? x) (equal? x (quote else)))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cdr e) table)))

(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
        (cons (meaning (car args) table)
              (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
      (meaning (car e) table)
      (evlis (cdr e) table))))

(define primitive? 
  (lambda (l)
    (equal? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (equal? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((equal? name (quote cons))
       (cons (first vals) (second vals)))
      ((equal? name (quote car))
       (car (first vals)))
      ((equal? name (quote cdr))
       (cdr (first vals)))
      ((equal? name (quote null?))
       (null? (first vals)))
      ((equal? name (quote equal?))
       (equal? (first vals) (second vals)))
      ((equal? name (quote atom?))
       (:atom? (first vals)))
      ((equal? name (quote zero?))
       (zero? (first vals)))
      ((equal? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((equal? (car x) (quote primitive)) #t)
      ((equal? (car x) (quote non-primitive)) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
      (extend-table
        (new-entry
          (formals-of closure)
           vals)
        (table-of closure)))))
