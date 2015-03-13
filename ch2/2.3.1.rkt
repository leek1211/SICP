#lang racket
(define nil '())

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(define (equal-list? l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
        ((null? l1) false)
        ((null? l2) false)
        (else (and (eq? (car l1) (car l2))
                   (equal-list? (cdr l1) (cdr l2))))))

(equal-list? '(this is a list) '(this is a list))
(equal-list? '(this is a list) '(this (is a) list))