#lang racket

(require memo)

(define (parse-splits)
  (define S #f)
  (define splits (make-hash))

  (for ([line (in-list (file->lines "input.txt"))]
        [i (in-naturals)])
    (for ([ch (in-string line)]
          [j (in-naturals)])
      (match ch
        [#\S (set! S (list i j))]
        [#\^ (hash-update! splits j (λ (x) (cons i x)) empty)]
        [else (void)])))
  (values
   S
   (hash-map/copy splits (λ (k v) (values k (reverse v))) #:kind 'immutable)))

(define-values (start splits) (parse-splits))

(define graph (make-hash))

(define (track to-track [tracked (set)] [result (set)])
  (cond
    [(empty? to-track) result]
    [(set-member? tracked (first to-track))
     (track (rest to-track) tracked result)]
    [else
     (match-define (list i j) (first to-track))
     (define lst (hash-ref splits j empty))
     (define split-point (findf (λ (x) (> x i)) lst))
     (cond
       [split-point
        (define left (list split-point (sub1 j)))
        (define right (list split-point (add1 j)))
        (hash-set! graph (first to-track) (list left right))
        (track (cons left (cons right (rest to-track)))
               (set-add tracked (first to-track))
               (set-add result (list split-point j)))]
       [else
        (track (rest to-track) (set-add tracked (first to-track)) result)])]))

;; part one
(set-count (track (list start)))

(define (children from)
  (hash-ref graph from (list 'root)))

(define/memoize (how-many? from)
                (if (eq? from 'root)
                    1
                    (for/sum ([child (in-list (children from))])
                             (how-many? child))))

;; part two
(how-many? start)
