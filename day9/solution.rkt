#lang racket

(require data/heap
         threading)

(define (parse-line line)
  (~> line (string-split ",") (map string->number _)))

(define data (~> "input.txt" file->lines (map parse-line _) list->vector))

(define N (vector-length data))

(define (area p1 p2)
  (match-define (list x1 y1) p1)
  (match-define (list x2 y2) p2)
  (* (add1 (abs (- x2 x1))) (add1 (abs (- y2 y1)))))

(define (node>? lhs rhs)
  (> (first lhs) (first rhs)))

(define h (make-heap node>?))

(for* ([i (in-range N)]
       [j (in-range (add1 i) N)])
  (define p1 (vector-ref data i))
  (define p2 (vector-ref data j))
  (heap-add! h (list (area p1 p2) (list i j))))

;; part 1
(first (heap-min h))

(define sides
  (cons (list (sub1 N) 0)
        (for/list ([i (in-range N)]
                   [j (in-range 1 N)])
          (list i j))))

(define-values (vertical-sides horisontal-sides)
  (for/fold ([v empty]
             [h empty])
            ([side (in-list sides)])
    (match-define (list x1 y1) (vector-ref data (first side)))
    (match-define (list x2 y2) (vector-ref data (second side)))
    (values (if (= x1 x2)
                (cons side v)
                v)
            (if (= y1 y2)
                (cons side h)
                h))))

(define (point-within? p)
  (match-define (list px py) p)
  (define on-the-edge?
    (for/or ([side (in-list sides)])
      (match-define (list x1 y1) (vector-ref data (first side)))
      (match-define (list x2 y2) (vector-ref data (second side)))
      (or (and (= x1 x2 px) (<= (min y1 y2) py (max y1 y2)))
          (and (= y1 y2 py) (<= (min x1 x2) px (max x1 x2))))))
  (if on-the-edge?
      #t
      (let ([px (+ px 0.5)]
            [py (+ py 0.5)])
        (odd? (for/sum
               ([side (in-list vertical-sides)])
               (match-define (list x1 y1) (vector-ref data (first side)))
               (match-define (list x2 y2) (vector-ref data (second side)))
               (if (and (< px x1) (<= (min y1 y2) py (max y1 y2))) 1 0))))))

(define (lines-intersect? h v)
  (match-define (list (list h-x1 h-y) (list h-x2 h-y)) h)
  (match-define (list (list v-x v-y1) (list v-x v-y2)) v)
  (match-define (list xmin xmax) (sort (list h-x1 h-x2) <))
  (match-define (list ymin ymax) (sort (list v-y1 v-y2) <))
  (and (<= ymin h-y ymax) (<= xmin v-x xmax)))

(define (rectangle-within? p1 p2)
  (match-define (list x1 y1) p1)
  (match-define (list x2 y2) p2)
  (match-define (list xmin xmax) (sort (list x1 x2) <))
  (match-define (list ymin ymax) (sort (list y1 y2) <))
  (define left (+ xmin 0.5))
  (define right (- xmax 0.5))
  (define bot (+ ymin 0.5))
  (define top (- ymax 0.5))
  (define hsides
    (list (list (list left top) (list right top))
          (list (list left bot) (list right bot))))
  (define vsides
    (list (list (list left top) (list left bot))
          (list (list right top) (list right bot))))
  (and (point-within? (list x1 y2))
       (point-within? (list x2 y1))
       (for*/and ([hside (in-list hsides)]
                  [vside (in-list vertical-sides)])
         (let ([vside (map (curry vector-ref data) vside)])
           (not (lines-intersect? hside vside))))
       (for*/and ([vside (in-list vsides)]
                  [hside (in-list horisontal-sides)])
         (let ([hside (map (curry vector-ref data) hside)])
           (not (lines-intersect? hside vside))))))

;; part 2
(for/first ([node (in-heap h)]
            #:do [(match-define (list a (list i j)) node)
                  (define p1 (vector-ref data i))
                  (define p2 (vector-ref data j))]
            #:when (rectangle-within? p1 p2))
  a)
