#lang racket

(require threading)

(define (parse-line line)
  (match-define (list region shapes) (string-split line ":"))
  (list (map string->number (string-split region "x"))
        (map string->number (string-split shapes))))

(define (parse-shapes text)
  (define (parse-shape chunk)
    (~> chunk (string-split "\n") rest (string-join "") string->list))
  (map parse-shape (string-split text "\n\n")))

(define lines (map parse-line (file->lines "input.txt")))
(define shapes (parse-shapes (file->string "shapes.txt")))
(define areas (map (λ (ls) (count (curry char=? #\#) ls)) shapes))

(define (area-check line)
  (match-define (list (list N M) counts) line)
  (define area (for/sum ([n (in-list counts)] [a (in-list areas)]) (* n a)))
  (<= area (* N M)))

(define (naive-box-check line)
  (match-define (list (list N M) counts) line)
  (define boxes (* (quotient N 3) (quotient M 3)))
  (<= (foldl + 0 counts) boxes))

(for/sum ([line (in-list lines)] #:when (and (area-check line)
                                             (naive-box-check line)))
         1)
