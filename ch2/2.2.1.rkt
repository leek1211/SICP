#lang racket
(define nil '())
(define (last-pair list1)
  (if (null? (cdr list1))
    list1
    (last-pair (cdr list1))))

(define (reverse2 list1)
    (define (helper list1 answer)
      (if (null? list1)
        answer
        (helper (cdr list1) (cons (car list1) answer))))
    (helper list1 nil))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (no-more? k)
  (null? k))
(define (except-first-denomination k)
  (cdr k))
(define (first-denomination k)
  (car k))

(define (cc  amount coin-values)
  (cond ((= amount 0 ) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else 
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount 
                    (first-denomination coin-values))
                 coin-values)))))

(define (same-parity x . y)
  (define (f list1)
    (if (null? list1)
      null

      (if (= (remainder x 2)
             (remainder (car list1) 2))
        (cons (car list1) (f (cdr list1) ))
        (f (cdr list1)))))
  (cons x (f y)))


(define (square-list list1)
  (map (lambda (x) (* x x))
       list1))

(define (test1 x)
  (cons 1 x))
(define (test2 x)
  (cons x 1))
(define (my-for-each proc list1)
  (if (null? list1)
    true
    (and (proc (car list1)) (my-for-each proc (cdr list1)))))
  

(my-for-each (lambda (x) (newline) (display x))
             (list 57 321 88))
(test1 (list 1 2 3 4))
(test2 (list 1 2 3 4))
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))
(list-ref squares 3)
(length odds)
(last-pair squares)
(reverse squares)
(reverse2 squares)
(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(square-list (list 1 2 3 4 7))
