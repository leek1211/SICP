#lang racket
(define nil '())
(define (count-leaves x)
  (cond ((null? x) 0 )
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(cons(list 1 2) (list 3 4))
(list 1 (list 2 ( list 3 4)))
(define l1 (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr l1 )))))
(define l2 (list (list 7)))
l2
(car (car l2))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
l3
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)
(define (deep-reverse input)
  (cond ((null? input) nil)
        ((pair? input) (append (deep-reverse (cdr input)) (cons (deep-reverse (car input)) nil)))
        (else input)))

(define ll (list (list 1 2) (list 3 4)))
(reverse ll)
(deep-reverse ll)
(deep-reverse (list 1 2))
(list x 3 4 y)
(deep-reverse (list x 3 4 y))
(define (fringe input)
  (cond ((null? input) input)
        ((not (pair? input)) (cons input nil))
        (else (append (fringe (car input)) (fringe (cdr input))))))

(define x1 (list (list 1 2) (list 3 4)))

(fringe x1)
(fringe (list x1 x1))

(define (make-mobile left right)
  (list left right))
(define (make-branch len structure)
  (list len structure))

(define (left-branch mob)
  (car mob))
(define (right-branch mob)
  (car (cdr mob)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
  
(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile))))))

(define (balanced mobile)
  (if (not (pair? mobile))
    true
    ((and (= (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
          (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))
          (balanced (branch-structure (left-branch mobile)))
          (balanced (branch-structure (right-branch mobile)))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (square x) 
  (* x x))

(define (square-tree2 tree) (tree-map square tree))
(square-tree2 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (elem) (cons (car s) elem)) rest)))))

(subsets (list 1 2 3))
