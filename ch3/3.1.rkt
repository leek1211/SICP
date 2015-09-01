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

(define (make-joint acc pin new-pin)



(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (sq num)
  (* num num))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (counter tt)
    (cond ((= tt 0) 0)
          ((P (random-in-range x1 x2) (random-in-range y1 y2)) (+ 1 (counter (- tt 1))))
          (else (counter (- tt 1)))))
  (define area (* 1.0 (- x2 x1) (- y2 y1)))
  (define ratio (/ (* 1.0 (counter trials)) trials))
  (* area ratio))


(define (ppp x y)
  (<= (+ (sq x) (sq y)) 1.0))
(estimate-integral ppp -1 1 -1 1 100000000)




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






