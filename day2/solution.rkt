#lang racket

(require threading)

(define ranges (~> "input.txt" file->string string-trim (string-split ",")))

(define (unpack r)
  (map string->number (string-split r "-")))

(define (invalid-id? id)
  (define s (number->string id))
  (define len (string-length s))
  (cond
    [(even? (string-length s))
     (define lhs (substring s 0 (/ len 2)))
     (define rhs (substring s (/ len 2)))
     (string=? lhs rhs)]
    [else #f]))

(define (divisible? n m)
  (zero? (remainder n m)))

(define (all-equal? lst)
  (for/and ([x lst]
            [y (rest lst)])
    (equal? x y)))

(define (invalid-id-v2? id)
  (define s (number->string id))
  (define len (string-length s))
  (for/or ([sz (in-inclusive-range 1 (quotient len 2))]
           #:when (divisible? len sz))
    (all-equal? (for/list ([i (in-range 0 len sz)])
                  (substring s i (+ i sz))))))

(define (solve pred?)
  (for/sum ([r ranges])
           (match-define (list from to) (unpack r))
           (for/sum ([i (in-inclusive-range from to)] #:when (pred? i)) i)))

(solve invalid-id?)
(solve invalid-id-v2?)
