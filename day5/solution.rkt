#lang racket

(require threading)

(define (parse-range line)
  (~> line
      (string-split "-")
      (map string->number _)))

(define ranges
  (~> "ranges.txt"
      file->lines
      (map parse-range _)
      (sort #:key first <)))

(define ids (map string->number (file->lines "input.txt")))

(define (in-range? x r)
  (match-define (list from to) r)
  (<= from x to))

(define (is-fresh? x) (ormap (curry in-range? x) ranges))

;; part 1
(for/sum ([id (in-list ids)] #:when (is-fresh? id)) 1)

(define (intersect? lhs rhs)
  (match-define (list x1 y1) lhs)
  (match-define (list x2 y2) rhs)
  (and (>= y1 x2) (>= y2 x1)))

(define (merge lhs rhs)
  (match-define (list x1 y1) lhs)
  (match-define (list x2 y2) rhs)
  (list (min x1 x2) (max y1 y2)))

(define merged
  (let loop ([cur (first ranges)][rs (rest ranges)][acc empty])
    (cond
      [(empty? rs) (cons cur acc)]
      [(intersect? cur (first rs))
       (let ([cur (merge cur (first rs))])
         (loop cur (rest rs) acc))]
      [else (loop (first rs) (rest rs) (cons cur acc))])))

(define (range-count r)
  (match-define (list from to) r)
  (add1 (- to from)))

;; part 2
(apply + (map range-count merged))
