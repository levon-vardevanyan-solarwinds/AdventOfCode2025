#lang racket

(require data/queue)
(require threading)

(define lines (file->lines "input.txt"))

(define (parse-line line)
  (match-define (list _ lights etc joltage)
    (regexp-match #px"^\\[([^]]+)\\](.+)\\{([^}]+)\\}$" line))
  (define buttons
    (~> etc
        (regexp-match* #px"\\(([^)]+)\\)" _ #:match-select second)
        (map (curryr string-split ",") _)))
  (list lights
        (for/vector ([button (in-list buttons)])
          (list->set (map string->number button)))
        (map string->number (string-split joltage ","))))

(define (encode-lights lights)
  (for/fold ([acc 0])
            ([ch (in-string lights)]
             [i (in-naturals)]
             #:when (char=? ch #\#))
    (bitwise-ior acc (arithmetic-shift 1 i))))

(define (f-button button)
  (define mask
    (for/fold ([acc 0]) ([i (in-set button)])
      (bitwise-ior acc (arithmetic-shift 1 i))))
  (curryr bitwise-xor mask))

(define (BFS goal f-buttons)
  (define N (vector-length f-buttons))
  (define q (make-queue))
  (for ([i (in-range N)])
    (enqueue! q (list 0 i empty)))
  (let loop ([visited (set)])
    (unless (queue-empty? q)
      (match-define (list cur next history) (dequeue! q))
      (cond
        [(set-member? visited (cons cur next)) (loop visited)]
        [(= cur goal) history]
        [else
         (define ncur ((vector-ref f-buttons next) cur))
         (for ([i (in-range N)])
           (enqueue! q (list ncur i (cons next history))))
         (loop (set-add visited (cons cur next)))]))))

;; part 1
(for/sum ([line lines])
         (match-define (list lights buttons joltage) (parse-line line))
         (define f-buttons (vector-map f-button buttons))
         (length (BFS (encode-lights lights) f-buttons)))

(define (lp-solve buttons joltage)
  (define N (length joltage)) ; equasion number
  (define M (vector-length buttons)) ; parameter number
  (define params
    (for/list ([i (in-range M)])
      (format "x~a" i)))
  ; Objective function
  (define objective (format "min: ~a;" (string-join params " + ")))
  (define constraints
    (for/list ([sum (in-list joltage)]
               [i (in-range N)])
      (~> (for/list ([st (in-vector buttons)]
                     [j (in-naturals)]
                     #:when (set-member? st i))
            (format "x~a" j))
          (string-join " + ")
          (format "~a = ~a;" _ sum))))
  ; Variable types (all parameters must be integral)
  (define types (format "int ~a;" (string-join params ", ")))
  (define program
    (string-join (list objective (string-join constraints "\n") types) "\n"))
  (define exe "/opt/homebrew/bin/lp_solve")
  (define output
    (with-output-to-string
     (λ () (system (format "echo \"~a\" | ~a -S1 /dev/stdin" program exe)))))
  (~> output
      (regexp-match #px"Value of objective function: ([^.]+)." _)
      second
      string->number))

;; part 2
(for/sum ([line lines])
         (match-define (list lights buttons joltage) (parse-line line))
         (lp-solve buttons joltage))
