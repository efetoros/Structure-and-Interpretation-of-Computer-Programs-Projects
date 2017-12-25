(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (cond 
    ((eq? nil items) items)
    (else (cons (proc (car items)) (map proc (cdr items))))
    ))

(define (zip2 pairs)
  (list (map car pairs) (map cadr pairs)))




  



(define (cons-all first rests)
  (cond
    ((eq? rests nil) rests)
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))


    )

  )

(define (zip list1 list2)
  (cond 
  ((or (eq? nil list1) (eq? nil list2)) nil)
  (else (cons(list (car list1 ) (car list2)) (zip (cdr list1) (cdr list2)) ))
    ))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
    (define (index x y)
      (cond
        ((eq? (car x) y) 0)
        (else (+ 1 (index (cdr x) y)))
            ))
      (define (indexlst copylst lst) 
        (cond
          ((eq? nil lst) lst)
          (else (cons (index copylst (car lst)) (indexlst copylst (cdr lst)))) 

          )
        )
      (zip (indexlst s s) s)
    
      )
  
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((eq? denoms nil) nil)
    ((eq? total 0) (list nil)) 
    ((> (car denoms) total) (list-change total (cdr denoms)))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))


    )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
          expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
           expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons
            (cons 'lambda
            (cons (car (zip2 (let-to-lambda values))) (let-to-lambda body)))
            (cadr (zip2 (let-to-lambda values)))
              )
           ; (cons
           ;    (cons 'lambda (cons (car (zip (car values) (cadr values))) (let-to-lambda body)))
           ;    (cadr (zip (car values) (cadr  values)))
           ;          )
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
          (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
