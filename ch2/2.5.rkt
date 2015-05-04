#lang racket

; In complex package, procedure make-from-real-imag and make-from-mag-ang call rectangular package and polar package.
; Therefore, the call of magnitude in 'complex type will call polar or rectangular package's magnitude operation.

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    ((if (number? datum)
       'scheme-number
       (error "bad tag" datum)))))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (define (raise x)
    ((get 'make 'rational) x 1))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise 'scheme-number
       (lambda (x) ((get 'make 'rational) x 1)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and 
      (eq? (real-part z1) (real-part z2))
      (eq? (imag-part z1) (imag-part z2))))
  (define (=zero? z1)
    (eq? 0 (mag z1)))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  'done)
(define (=zero? x) (apply-generic '=zero? x))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (higher x y)
  (cond ((eq? (type-tag x) (type-tag y)) false)
        ((not (get 'raise y)) false)
        ((if (eq? (type-tag x) (type-tag ((get 'raise (type-tag y)) y)))
           true
           ((higher x ((get 'raise (type-tag y)) y)))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (cond ((= (length args) 2)
               (let ((type1 (car type-tags))
                     (type2 (cadr type-tags))
                     (a1 (car args))
                     (a2 (cadr args)))
                 (let ((t1->t2 (get-coercion type1 type2))
                       (t2->t1 (get-coercion type2 type1)))
                   (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                         (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                         (else
                           (error "No method for these types"
                                  (list op type-tags)))))))
              ((> (length args) 3)
               (let ((second ((apply-generic op (cdr args))))
                     (type1 (car type-tags))
                     (type2 (type-tag second))
                     (a1 (car args))
                     (a2 (second)))
                 (let ((t1->t2 (get-coercion type1 type2))
                       (t2->t1 (get-coercion type2 type1)))
                   (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                         (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                         (else
                           (error "No method for these types"
                                  (list op type-tags)))))))


              (else error "No method for these types"
                    (list op type-tags)))))))
;2.82 the method won't work, because there is a case where each type of result cab be lowered, and then applied to the next proc.
;ex. (+ sqrt(2)+1 -sqrt(2)+1 2.5)
