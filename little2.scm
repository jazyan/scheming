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
