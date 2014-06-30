; operations

(define add1 
 (lambda (n)
  (+ n 1)))

(define sub1
 (lambda (n)
  (- n 1)))

(define addn
 (lambda (a b)
  (cond
   ((zero? b) a)
   (else (add1 (addn a (sub1 b)))))))

(define subn
 (lambda (a b)
  (cond
   ((zero? b) a)
   (else (sub1 (subn a (sub1 b)))))))

(define addtup
 (lambda (tup)
  (cond
   ((null? tup) 0)
   (else (addn (car tup) (addtup (cdr tup)))))))

(define multn
 (lambda (a b)
  (cond
   ((zero? b) 0)
    (else (addn a (multn a (sub1 b)))))))

; adding elements of 2 tuples
(define tup+
 (lambda (tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (else (cons (addn (car tup1) (car tup2)) 
            (tup+ (cdr tup1) (cdr tup2)))))))

; compare
(define gtr
 (lambda (a b)
  (cond
   ((zero? a) #f)
   ((zero? b) #t)
   (else (gtr (sub1 a) (sub1 b))))))

(define lsr
 (lambda (a b)
  (cond
   ((zero? b) #f)
   ((zero? a) #t)
   (else (lsr (sub1 a) (sub1 b))))))

(define eq
 (lambda (a b)
  (cond
   ((zero? a) (zero? b))
   ((zero? b) #f)
   (else (eq (sub1 a) (sub1 b))))))

(define eq2
 (lambda (a b)
  (cond
   ((lsr a b) #f)
   ((gtr a b) #f)
   (else #t))))

(define raise
 (lambda (a b)
  (cond 
   ((zero? b) 1)
   (else (multn a (raise a (sub1 b)))))))

(define divfl
 (lambda (a b)
  (cond
   ((lsr a b) 0)
   (else (add1 (divfl (subn a b) b))))))

; length of list
(define len
 (lambda (lat)
  (cond
   ((null? lat) 0)
   (else (add1 (len (cdr lat)))))))

; returns nth element in lat
(define pick
 (lambda (n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else (pick (sub1 n) (cdr lat))))))
         ; removes nth element from lat
(define rempick
 (lambda (n lat)
  (cond
   ((one? n) (cdr lat))
   (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
         ; removes nums from lat
(define no-nums
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((number? (car lat)) (no-nums (cdr lat)))
   (else (cons (car lat) (no-nums (cdr lat)))))))

; extracts tuple from lat
(define all-nums
 (lambda (lat)
  (cond
   ((null? lat) (quote ()))
   ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
   (else (all-nums (cdr lat))))))

; checks whether two atoms same
(define eqan? 
 (lambda (a1 a2)
   (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (equal? a1 a2)))))

(define eqlist?
 (lambda (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) #t)
   ((or (null? l1) (null? l2)) #f)
   ((and (atom? (car l1)) (atom? (car l2))) 
         (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   ((or (atom? (car l1)) (atom? (car l2))) #f)
   (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; number of times an atom occurs in list
(define occur
 (lambda (a lat)
  (cond
   ((null? lat) 0)
   ((equal? a (car lat)) (add1 (occur a (cdr lat))))
   (else (occur a (cdr lat))))))

; number of times an atom occurs in a list of lists
(define occur*
 (lambda (a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((equal? a (car l)) (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l)))))
   (else (addn (occur* a (car l)) (occur* a (cdr l)))))))

; true if one, else false
(define one? 
  (lambda (n)
    (= n 1)))

; removes first instance of a in lat
(define rember
 (lambda (a lat)
  (cond 
    ((null? lat) (quote ()))
    ((equal? (car lat) a) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat)))))))

; removes all a in lat
(define multirember
 (lambda (a lat)
  (cond 
    ((null? lat) (quote ()))
    ((equal? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond 
         ((equal? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; doesn't work for empty lists
(define firsts
 (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((equal? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
  
;inserts new to the right of all olds
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((equal? (car lat) old) 
 (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;insertR for list of lists or empty lists
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond 
         ((equal? (car l) old) 
            (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define insertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((equal? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond 
         ((equal? (car l) old) 
          (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((equal? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((equal? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote ()))
      ((or (equal? (car lat) o1) (equal? (car lat) o2)) 
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define subst*
  (lambda (new old l)
   (cond 
     ((null? l) (quote ()))
     ((atom? (car l)) 
      (cond
        ((equal? old (car l)) (cons new (subst* new old (cdr l))))
        (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define member*
 (lambda (a l)
  (cond 
     ((null? l) (quote #f))
     ((atom? (car l)) (or (equal? a (car l)) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost*
  (lambda (l)
    (cond 
      ((atom? (car l)) (car l))
      (else (leftmost* (car l))))))
