#lang racket
(define nil '())
(define (make-vector x y)
  (cons x y))
(define (add-vector v1 v2)
  (cons (+ (car v1) (car v2))
        (+ (cdr v1) (cdr v2))))
(define (sub-vector v1 v2)
  (cons (- (car v1) (car v2))
        (- (cdr v1) (cdr v2))))

