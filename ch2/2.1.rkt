#lang racket

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b))))


(define (make-rat n d)
    
    (let ([g (gcd (abs n) (abs d))]
          [ns (/ n (abs n))]
          [ds (/ d (abs d))])
    (cons (* (/ (abs n) g) (* ns ds))  (/ (abs d) g) )))
          
(car (make-rat 3 -6))
(cdr (make-rat 3 -6))
