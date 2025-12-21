#lang racket

(require memo)
(require threading)

(define (parse-line line)
  (match-define (list from to) (string-split line ":"))
  (cons from (string-split to)))

(define dag (~> "input.txt" file->lines (map parse-line _) make-hash))

(define (draw-graph dag)
  (define (dot-gen)
    (define nodes
      (for/list ([(k v) (in-hash dag)])
        (format "~a -> { ~a }" k (string-join v " "))))
    (format "digraph {\n~a}" (string-join nodes ";\n")))
  (define dot.exe "/opt/homebrew/bin/dot")
  (system (format "echo '~a' | ~a -Tsvg -o graph.svg" (dot-gen) dot.exe)))

; (draw-graph dag)

(define (children from)
  (hash-ref dag from empty))

(define/memoize (how-many? from to)
                (if (string=? from to)
                    1
                    (for/sum ([child (in-list (children from))])
                             (how-many? child to))))

;; part 1
(how-many? "you" "out")

;; part 2
(+ (* (how-many? "svr" "dac") (how-many? "dac" "fft") (how-many? "fft" "out"))
   (* (how-many? "svr" "fft") (how-many? "fft" "dac") (how-many? "dac" "out")))
