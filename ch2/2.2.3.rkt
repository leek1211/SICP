#lang racket
(define nil '())
(define (square x)
  (* x x))
(define (identity x)
  x)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence ))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(accumulate * 1 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(append2 (list 1 2 3) (list 4 5 6))

(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 12 3 4 5 6 7 8 19 10))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves x)
  (cond ((null? x) 0 )
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
          (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define vec (list 1 2 3 4))
(matrix-*-vector mat vec)

(define (transpose m)
  (accumulate-n cons nil m))
(transpose mat)
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix mat (transpose mat))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-right / 1 (list 2 3))
(fold-right / 1 (list 2))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; REVERSE
(fold-right (lambda (x y) (append y (list x))) nil (list 1 2 3 4 5 6))
(fold-left (lambda (x y) (cons y x)) nil (list 1 2 3 4 5 6))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(map square (list 1 2 3))
(flatmap (lambda (i) (map square i)) (list (list 1) (list 2) (list 3)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))
(permutations (list 1 2 3))

(define (unique-pairs n) 
  (flatmap (lambda(i) (map (lambda (j) (list i j))
                           (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(unique-pairs 4)

(define (make-pair-sum pair)
       (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (even? (+ (car pair) (cadr pair)))) ; even? <= prime?

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (triples n s)
  (define (sums? ls)
    (= s (+ (car ls) (cadr ls) (caddr ls))))
  (filter sums? 
          (flatmap (lambda (i) (map (lambda (j) (cons i j))
                                    (unique-pairs (- i 1))))
                     (enumerate-interval 1 n))))
(triples 10 13)

(define (queens board-size)
  (define empty-board nil)
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (contains key ls)
    (cond ((null? ls) true)
          ((= key (car ls)) false)
          (else contains key (cdr ls))))

  (define (safe? k positions)
    (define cur (car positions))
    (define (safe-iter t b remain)
      (cond ((null? remain) true)
            ((or (= cur (car remain))
                 (= t (car remain))
                 (= b (car remain))) false)
            (else (safe-iter (- t 1) (+ b 1) (cdr remain)))))
    (safe-iter (- cur 1) (+ cur 1) (cdr positions)))

  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter 
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(queens 8)


