#lang racket

(require data/heap)
(require threading)

(define (parse-line line)
  (~> line
      (string-split ",")
      (map string->number _)))

(define data
  (~> "input.txt"
      file->lines
      (map parse-line _)
      list->vector))

(define N (vector-length data))

(define square (curryr expt 2))

(define (distance lhs rhs)
  (match-define (list x1 y1 z1) lhs)
  (match-define (list x2 y2 z2) rhs)
  (sqrt (+ (square (- x1 x2))
           (square (- y1 y2))
           (square (- z1 z2)))))

(define (node<=? lhs rhs)
  (<= (first lhs) (first rhs)))

(define h (make-heap node<=?))

(for ([i (in-range N)])
  (for ([j (in-range (add1 i) N)])
    (define p1 (vector-ref data i))
    (define p2 (vector-ref data j))
    (heap-add! h (list (distance p1 p2) (list i j)))))

; Disjoint-set/Union-Find forest *sigh*
(struct uff (p h) #:transparent)

; we start with N sets, each element acts as its own set representative
(define (make-uff N)
  (uff (build-vector N identity)
       (make-hash
        (for/list ([i (in-range N)])
          (cons i (list i))))))

; find the representative for the set that contains x
(define (uff-find uf x)
  (vector-ref (uff-p uf) x))

;; union set containing x with set containing y
(define (uff-union! uf x y)
  (define x-repr (uff-find uf x))
  (define y-repr (uff-find uf y))
  (define x-list (hash-ref (uff-h uf) x-repr))
  (define y-list (hash-ref (uff-h uf) y-repr))
  ; moving elements from the shortest set to the longest set
  ; to achieve better amortized complexity ()
  (let-values ([(x-repr y-repr x-list y-list)
                (if (> (length x-list) (length y-list))
                    (values y-repr x-repr y-list x-list)
                    (values x-repr y-repr x-list y-list))])
    (for ([id (in-list x-list)])
      (vector-set! (uff-p uf) id y-repr))
    (hash-set! (uff-h uf) y-repr (append x-list y-list))
    (hash-remove! (uff-h uf) x-repr)))

(define uf (make-uff N))

(for ([node (in-heap h)]
      [i (in-range 1000)]
      #:do [(match-define (list dist (list x y)) node)]
      #:unless (= (uff-find uf x) (uff-find uf y)))
  (uff-union! uf x y))

;; part 1
(~> uf
    uff-h
    (hash-map (λ (k v) (length v)))
    (sort >)
    (take 3)
    (foldl * 1 _))

(match-define
  (list x y)
  (for/last ([node (in-heap h)]
             [i (in-naturals)]
             #:do [(match-define (list dist (list x y)) node)]
             #:unless (= (uff-find uf x) (uff-find uf y))
             #:break (hash-empty? (uff-h uf)))
    (uff-union! uf x y)
    (list x y)))

;; part 2
(* (first (vector-ref data x))
   (first (vector-ref data y)))
