#lang racket

(require threading)

(define lines (file->lines "input.txt"))

(define N (length lines))
(define M (string-length (first lines)))

(define (in-bounds? posn)
  (match-define (list i j) posn)
  (and (>= i 0) (>= j 0) (< i N) (< j N)))

(define rolls (mutable-set))

(for ([line (in-list lines)][i (in-naturals)])
  (for ([ch (in-string line)][j (in-naturals)]
              #:when (char=? ch #\@))
    (set-add! rolls (list i j))))

(define (roll? posn)
  (set-member? rolls posn))

;; '((0 1) (0 -1) (1 0) (1 1) (1 -1) (-1 0) (-1 1) (-1 -1))
(define deltas
  (~>
   (cartesian-product '(0 1 -1) '(0 1 -1))
   (remove '(0 0) _)))

(define (adjacent posn)
  (~>
   deltas
   (map (λ (D) (map + D posn)) _)
   (filter in-bounds? _)))

(define (to-remove rolls)
  (for/set ([roll (in-set rolls)]
             #:when (< (count roll? (adjacent roll)) 4))
    roll))

;; part one
(set-count (to-remove rolls))

;; part two
(for/sum ([i (in-naturals)]
           #:do [(define tr (to-remove rolls))
                 (define len (set-count tr))]
           #:break (zero? len))
  (set-subtract! rolls tr) len)
