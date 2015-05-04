#lang racket
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-account-pw balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define call-the-cops "POLICE!")
  (let ((wrong 0))
    (define (dispatch pin m)
      (if (eq? pin passwd)
        (begin (set! wrong 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (begin (set! wrong (+ wrong 1))
               (if (< wrong 7)
                 (lambda (x) "Incorrect password")
                 (lambda (x) call-the-cops)))))
  dispatch))

(define (make-accumulator cur)
  (lambda (num)
    (begin (set! cur (+ cur num))
           cur)))

(define (make-monitored f)
  (let ((numcall 0))
    (lambda (m)
      (if (eq? m 'how-many-calls?)
        numcall
        (begin (set! numcall (+ numcall 1)) (f m))))))

(define acc1 (make-account-pw 100 '123))

((acc1 'hihi 'withdraw) 50)
((acc1 '123 'deposit) 20)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)
((acc1 'hihi 'withdraw) 50)






