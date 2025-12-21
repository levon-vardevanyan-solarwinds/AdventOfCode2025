#lang racket

(require threading)

(define lines (file->lines "input.txt"))

(define (digit->number ch)
  (- (char->integer ch) (char->integer #\0)))

(define (partial-first line)
  (define len (string-length line))
  (define last (~> line (string-ref (sub1 len)) digit->number))
  (define partial (make-vector len last))
  (for ([i (in-inclusive-range (- len 2) 0 -1)])
    (define x (digit->number (string-ref line i)))
    (vector-set! partial i (max (vector-ref partial (add1 i)) x)))
  partial)

(define (partial-next line prev)
  (define len (vector-length prev))
  (define last-prev (vector-ref prev (sub1 len)))
  (define power (add1 (order-of-magnitude last-prev)))
  (define (concat hi lo)
    (+ (* (expt 10 power) hi) lo))
  (define digit (~> line (string-ref (- len 2)) digit->number))
  (define partial (make-vector (sub1 len) (concat digit last-prev)))
  (for ([i (in-inclusive-range (- len 3) 0 -1)])
    (define x (digit->number (string-ref line i)))
    (define n (concat x (vector-ref prev (add1 i))))
    (vector-set! partial i (max (vector-ref partial (add1 i)) n)))
  partial)

(define (max-joltage-n line n)
  (~> (for/fold ([acc (partial-first line)]) ([i (in-range 1 n)])
        (partial-next line acc))
      (vector-argmax identity _)))

(define (solve n)
  (apply + (map (curryr max-joltage-n n) lines)))

(solve 2)
(solve 12)
