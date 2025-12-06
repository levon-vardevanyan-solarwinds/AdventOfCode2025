#lang racket

(require threading)

(match-define
  (list head ... tail)
  (file->lines "input.txt"))

(define ops
  (map
   (λ (s) (match s ["+" +]["*" *]))
   (string-split tail)))

(define (part-1)
  (define nums
    (map 
     (λ~>
      string-split
      (map string->number _)
      list->vector)
     head))

  (for/sum ([op (in-list ops)]
            [i (in-naturals)])
    (apply op (map (curryr vector-ref i) nums))))

(define N (length head))
(define M (string-length (first head)))

(define (build-number lst)
  (~> lst
      (filter (negate (curry char=? #\space)) _)
      list->string
      string->number))

(define (part-2)
  (define nums
    (for/fold ([cur empty][acc empty] #:result (cons cur acc))
              ([i (in-inclusive-range (sub1 M) 0 -1)])
      (define n (build-number (map (curryr string-ref i) head)))
      (if n
          (values (cons n cur) acc)
          (values empty (cons cur acc)))))

  (for/sum ([op (in-list ops)]
            [args (in-list nums)])
    (apply op args)))

(part-1)
(part-2)
