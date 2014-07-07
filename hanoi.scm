; start and end positions
(define s '((1 2 3) () ()))
(define e '(() () (1 2 3)))
(define n 3)

(define test '((1 2 3) (4 5) (6 7 8)))

(define A (lambda (x) (car x)))
(define B (lambda (x) (car (cdr x))))
(define C (lambda (x) (car (cdr (cdr x)))))

(define valid?
  (lambda (move start end board)
    (cond
      ((equal? move (car (start board))) 
       (cond
         ((null? (end board)) #t)
         ((< move (car (end board))) #t)))
      (else #f))))

(define pop
  (lambda (move rung board)
    (cond
      ((equal? rung A) (cons (cdr (A board)) (cdr board)))
      ((equal? rung B) 
       (cons (A board) (cons (cdr (B board)) (cons (C board) (quote ())))))
      ((equal? rung C) 
       (cons (A board) (cons (B board) (cons (cdr (C board)) (quote ()))))))))

(display (pop 1 A test))

(define insert
  (lambda (move rung board)
    (cond
      ((equal? rung A) (cons (cons move (A board)) (cdr board)))
      ((equal? rung B) 
       (cons (A board) (cons (cons move (B board)) (cons (C board) (quote ())))))
      ((equal? rung C) 
       (cons (A board) (cons (B board) (cons (cons move (C board)) (quote ()))))))))

(display "\n")
(display (insert 1 C test))
(display "\n")

(define update
  (lambda (move start end board)
    (and (pop move start board) (insert move end board))))

(define stack?
  (lambda (board len end)
    (cond
      ((equal? (length (end board)) len) #t)
      (else #f))))

(define gameover?
  (lambda (board)
    (stack? board n C)))

(define hanoi
  (lambda (move start end use board)
    (cond
      ((gameover? board) (display board))
      ((equal? move 1) (display board))
      ((valid? move start end board) 
       (cond
         ((stack? move end) 
          (hanoi (+ 1 move) start use end (update move start end board)))
         (else (hanoi (- 1 move) start use end (update move start end board)))))
      (else (and (display move) (hanoi (- 1 move) start use end board))))))

(hanoi 2 A C B s)
