#lang racket

(define lines (file->lines "input.txt"))

(define (rotate x delta)
  (let* ([x (+ x delta)]
         [x (remainder x 100)]
         [x (if (negative? x) (+ x 100) x)])
    (values x (if (zero? x) 1 0))))

(define (rotate-brute x delta)
  (define step (if (negative? delta) sub1 add1))
  (for/fold ([cur x][zeros 0])
            ([i (in-range (abs delta))])
    (define stepped (step cur))
    (define next (match stepped [-1 99][100 0][else stepped]))
    (values next (if (zero? next) (add1 zeros) zeros))))

(define (rotate-smart x delta)
  (define sum (+ x delta))
  (define cycles (abs (quotient sum 100)))
  (define-values (next delta-zeros) (rotate x delta))
  (define zeros
    (cond
      [(or (zero? x) (positive? sum)) cycles]
      [(negative? sum) (+ cycles 1)]
      [else delta-zeros]))
  (values next zeros))

(define (solve f)
  (for/fold ([cur 50][zeros 0] #:result zeros)
            ([line lines])
    (define distance (string->number (substring line 1)))
    (define direction (match (string-ref line 0) [#\L -1][#\R  1]))
    (define-values (new-cur delta-zeros) (f cur (* distance direction)))
    (values new-cur (+ zeros delta-zeros))))

(solve rotate)
(solve rotate-brute)
(solve rotate-smart)
