;;; Copyright 2021 Indigo BioAutomation, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(import
 (chezscheme)
 (swish imports)
 (swish mat)
 (swish stream)
 )

(define (square x) (* x x))

(mat eos ()
  (assert (eos? eos)))

(mat stream ()
  (match-let* ([() (stream->list (stream))]
               [(1 2 3) (stream->list (stream 1 2 3))]
               [(1 2 3) (stream->list (list->stream '(1 2 3)))]
               [#() (stream->vector (stream))]
               [#(1 2 3) (stream->vector (stream 1 2 3))])
    'ok))

(mat transformers ()
  (match-let* ([(1 4 9) (s/> '(1 2 3) (s/map square))]
               [(1 3) (s/> '(1 2 3 4) (s/filter odd?))]
               [(1 3) (s/> '(1 #f 3 #f) (s/filter))]
               [6 (s/> '(1 2 3) (s/fold-right + 0))]
               [6 (s/> '(1 2 3) (s/fold-left + 0))]
               [(1 2 3) (s/> '(1 2 3) (s/fold-right cons '()))]
               [(3 2 1) (s/> '(1 2 3) (s/fold-left (lambda (ls x) (cons x ls)) '()))]
               [(3 2 1) (s/> '(1 2 3) s/reverse)]
               [() (s/> '(1 2 3) (s/take 0))]
               [(1 2) (s/> '(1 2 3) (s/take 2))]
               [(1 2 3) (s/> '(1 2 3) (s/take 3))]
               [(1 2 3) (s/> '(1 2 3) (s/take 4))]
               [(1 2 3) (s/> '(1 2 3) (s/drop 0))]
               [(3) (s/> '(1 2 3) (s/drop 2))]
               [() (s/> '(1 2 3) (s/drop 3))]
               [() (s/> '(1 2 3) (s/drop 4))]
               [() (s/> '() s/concat)]
               [(1 2 3 4) (s/> '((1 2) (3 4)) s/concat)]
               [(1 2 (3) 4 5) (s/> '(() () (1 2) () () ((3)) (4 5) () ()) s/concat)]
               [(1 2 3 4 5 6) (s/> (list '(1 2) '#(3 4) (stream 5 6)) s/concat)]
               [`(catch #(invalid-stream 1)) (try (s/> '(1) s/concat))]
               [,comma-join (s/join ", ")]
               ["" (s/> '() comma-join)]
               ["a" (s/> '(a) comma-join)]
               ["a, b" (s/> '(a b) comma-join)]
               ["a, b, c" (s/> '(a b c) comma-join)]
               [,fancy-join (s/join ", " " and " ", and ")]
               ["" (s/> '() fancy-join)]
               ["a" (s/> '(a) fancy-join)]
               ["a and b" (s/> '(a b) fancy-join)]
               ["a, b, and c" (s/> '(a b c) fancy-join)]
               ["a, b, c, and d" (s/> '(a b c d) fancy-join)]
               [(0 1 0 1 2) (s/> '(2 3) (s/map-concat iota))]
               [() (s/> '() (s/uniq))]
               [("a" "b") (s/> '("a" "b" "a") (s/uniq))]
               [("a" "b") (s/> '("a" "b" "A") (s/uniq string-ci-hash string-ci=?))]
               [("a" "b") (s/> '("a" "b" "A") (s/uniq-by values string-ci-hash string-ci=?))]
               [(1 2) (s/> '((1) (2) (1)) (s/map-uniq car))]
               [("a" "b") (s/> '(("a") ("b") ("a")) (s/map-uniq car string-hash string=?))]
               [((1 1) (2 2 -2)) (s/> '(1 2 -2) (s/group-by abs) s/cells (s/sort-by car <))]
               [((1 . 1) (2 . -2)) (s/> '(1 -2) (s/by-key abs) s/cells (s/sort-by car <))]
               [`(catch #(duplicate-key 1 1 -1)) (try (s/> '(1 -1) (s/by-key abs) s/cells))]
               [() (s/> '() (s/chunk-every 2))]
               [((0)) (s/> (iota 1) (s/chunk-every 2))]
               [((0 1)) (s/> (iota 2) (s/chunk-every 2))]
               [((0 1) (2)) (s/> (iota 3) (s/chunk-every 2))]
               [((0 1) (2 3)) (s/> (iota 4) (s/chunk-every 2))]
               [((0 1) (2 3) (4)) (s/> (iota 5) (s/chunk-every 2))]
               [((0) (1)) (s/> (iota 2) (s/chunk-every 1))]
               [`(catch #(bad-arg s/chunk-every 0)) (try (s/chunk-every 0))]
               [2 (s/> '(2 3) s/first)]
               [#f (s/> '() s/first)]
               [3 (s/> '(2 3) s/last)]
               [#f (s/> '() s/last)]
               [0 (s/> '() s/length)]
               [1 (s/> '(a) s/length)]
               [2 (s/> '(a b) s/length)]
               [+nan.0 (s/> '() (s/mean))]
               [0 (s/> '(0) (s/mean))]
               [5 (s/> '(5) (s/mean))]
               [11/2 (s/> '(5 6) (s/mean))]
               [5.5 (s/> '(5 6.0) (s/mean))]
               [5.5 (s/> '((5) (6.0)) (s/mean car))]
               [3 (s/> '(1 2) s/sum)]
               [3.0 (s/> '(1 2.0) s/sum)]
               [#f (s/> '() (s/max))]
               [5 (s/> '(3 5 4) (s/max))]
               [-5 (s/> '(3 -5 4) (s/max-by abs))]
               ["c" (s/> '("a" "c" "b") (s/max string>?))]
               ["c" (s/> '("a" "c" "b") (s/max-by values string>?))]
               [#f (s/> '() (s/min))]
               [-2 (s/> '(-2 1 3) (s/min))]
               [1 (s/> '(-2 1 3) (s/min-by abs))]
               ["a" (s/> '("a" "c" "b") (s/min string<?))]
               ["a" (s/> '("a" "c" "b") (s/min-by values string<?))]
               [#f (s/> '() (s/extrema))]
               [(5 5) (s/> '(5) (s/extrema))]
               [(-2 3) (s/> '(-2 1 3) (s/extrema))]
               [("a" "c") (s/> '("a" "c" "b") (s/extrema string<? string>?))]
               [(1 3) (s/> '(-2 1 3) (s/extrema-by abs))]
               [(("a") ("c")) (s/> '(("a") ("c") ("b")) (s/extrema-by car string<? string>?))]
               [(-2 3) (s/> '((-2) (1) (3)) (s/map-extrema car))]
               [("a" "c") (s/> '(("a") ("c") ("b")) (s/map-extrema car string<? string>?))]
               [(1 3) (s/> '(1 3 4 5) (s/take-while odd?))]
               [(1 3) (s/> '(1 3) (s/take-while odd?))]
               [() (s/> '(2) (s/take-while odd?))]
               [(4 5) (s/> '(1 3 4 5) (s/drop-while odd?))]
               [() (s/> '(1 3 5) (s/drop-while odd?))]
               [(2 3) (s/> '(2 3) (s/drop-while odd?))]
               [(1 2 3) (stream->list (stream-cons 1 (stream 2 3)))]
               [(1) (stream->list (stream-cons 1 (stream)))]
               [5 (s/> '(2 4 5 6 7) (s/find odd?))]
               [#f (s/> '(2 4) (s/find odd?))]
               [1 (s/> '((1) (2)) (s/map-min car))]
               ["a" (s/> '(("a") ("b")) (s/map-min car string<?))]
               [2 (s/> '((1) (2)) (s/map-max car))]
               ["b" (s/> '(("a") ("b")) (s/map-max car string>?))]
               [#f (s/> '() (s/any?))]
               [#t (s/> '(#f) (s/any?))]
               [#f (s/> '() s/any)]
               [(1) (s/> '(1) s/any)]
               [(1 2) (s/> '(1 2) s/any)]
               [#f (s/> '(#f #f) (s/any? values))]
               [#t (s/> '(#f 1) (s/any? values))]
               [#t (s/> '() (s/none?))]
               [#f (s/> '(#f) (s/none?))]
               [#t (s/> '(#f #f) (s/none? values))]
               [#f (s/> '(#f 1) (s/none? values))]
               [#f (s/> '(#f #t) (s/all? values))]
               [#t (s/> '(#t #t) (s/all? values))]
               [#t (s/> '() (s/all? values))]
               [0 (s/> '() (s/count values))]
               [2 (s/> '(#t #f #t) (s/count values))]
               [16 (s/> '((7) (0) (8) (#t) (#f)) (s/count car))]
               [(1 2 3) (s/> '(3 2 1) (s/sort <))]
               [(("c") ("b") ("a")) (s/> '(("a") ("b") ("c")) (s/sort-by car string>?))]
               [(("a" 0 0 1 2) ("b" 0 1)) (s/> '(("a" . 1) ("b" . 2) ("a" . 3))
                                            (s/ht** car (lambda (x) (iota (cdr x))))
                                            s/cells
                                            (s/sort-by car string<?))]
               [(("a" 1 3) ("b" 2)) (s/> '(("a" . 1) ("b" . 2) ("a" . 3))
                                      (s/ht* car cdr)
                                      (s/sort-by car string<?))]
               [(("a" . -1) ("b" . -2)) (s/> '(("a" . 1) ("b" . 2))
                                          (s/ht car (lambda (x) (- (cdr x))))
                                          (s/sort-by car string<?))]
               [`(catch #(duplicate-key "a" 1 2))
                (try (s/> '(("a" . 1) ("A" . 2))
                       (s/ht (lambda (x) (string-downcase (car x))) cdr)))]
               [() (s/> '() (s/chunk odd?))]
               [((1 3)) (s/> '(1 3) (s/chunk odd?))]
               [((1 3) (2 4) (5 7)) (s/> '(1 3 2 4 5 7) (s/chunk odd?))]
               [(("one" "two") ("three") ("four" "five") ("six"))
                (s/> '("one" "two" "three" "four" "five" "six") (s/chunk string-length))]
               [() (s/> '() (s/chunk2 <=))]
               [((1)) (s/> '(1) (s/chunk2 <=))]
               [((1 2)) (s/> '(1 2) (s/chunk2 <=))]
               [((2 1)) (s/> '(2 1) (s/chunk2 <=))]
               [((2 1) (2)) (s/> '(2 1 2) (s/chunk2 <=))]
               [((2 3 4) (3 2 1) (2 3 4)) (s/> '(2 3 4 3 2 1 2 3 4) (s/chunk2 <=))]
               [((3 2 1) (2 3 4) (3 2 1)) (s/> '(3 2 1 2 3 4 3 2 1) (s/chunk2 <=))]
               [(1 2 3) (s/> '(1 2 3) (s/drop-last 0))]
               [(1 2) (s/> '(1 2 3) (s/drop-last 1))]
               [(1) (s/> '(1 2 3) (s/drop-last 2))]
               [() (s/> '(1 2 3) (s/drop-last 3))]
               [() (s/> '(1 2 3) (s/drop-last 4))]
               [() (s/> '() (s/drop-last 4))]
               [() (s/> '(1 2 3) (s/take-last 0))]
               [(3) (s/> '(1 2 3) (s/take-last 1))]
               [(2 3) (s/> '(1 2 3) (s/take-last 2))]
               [(1 2 3) (s/> '(1 2 3) (s/take-last 3))]
               [(1 2 3) (s/> '(1 2 3) (s/take-last 4))]
               [() (s/> '() (s/take-last 4))]
               [(("a" . 1) ("b" . 2)) (s/> '((a . 1) (b . 2)) (s/map-car symbol->string))]
               [((a . 2) (b . 3)) (s/> '((a . 1) (b . 2)) (s/map-cdr 1+))]
               [((1 . -1) (2 . -2)) (s/> '(1 2) (s/map-cons -))]
               [((a . 0) (b . 1) (c . 2)) (s/> '(a b c) (s/map-index cons))]
               [`(catch #(stream-length-mismatch c)) (try (s/> '(a b c) (s/zip '(0 1) cons)))]
               [((a b) (b c) (c d)) (s/> '(a b c d) (s/map-window 2 values))]
               [((c b a) (d c b)) (s/> '(a b c d) (s/map-window 3 reverse))]
               [((a) (b) (c)) (s/> '(a b c) (s/map-window 1 values))]
               [`(catch #(bad-arg s/map-window 0)) (try (s/map-window 0 values))]
               )
    'ok))

(mat reapply-transformers ()
  (let ([take2 (s/take 2)]
        [drop2 (s/drop 2)]
        [uniqt (s/uniq)])
    (match-let* ([(1 2) (s/> '(1 2 3) take2)]
                 [(4 5) (s/> '(4 5 6) take2)]
                 [(3 4) (s/> '(1 2 3 4) drop2)]
                 [(7 8) (s/> '(5 6 7 8) drop2)]
                 [(1) (s/> '(1 1) uniqt)]
                 [(1) (s/> '(1 1) uniqt)]
                 )
      'ok)))

(mat hashtable-type-inference ()
  (define-record-type foo (fields a))
  (match-let* ([(a b) (s/> '(a b a) (s/uniq))]
               [(1 2) (s/> '(1 2 1) (s/uniq))]
               [(,a ,b) (list (make-foo 1) (make-foo 2))]
               [(,@a ,@b) (s/> (list a b a) (s/uniq))]
               [("a" "b") (s/> (list "a" "b" "a") (s/uniq))]
               [(1.1 2.2) (s/> (list 1.1 2.2 1.1) (s/uniq))]
               )
    'ok))

(mat hashtables ()
  (define ht (s/> '((a . 1) (b . 2)) (s/ht car cdr)))
  (match-let* ([(a b) (s/> ht (s/map car))]
               [(a b) (s/> ht s/cells (s/map car))]
               [((1 . a) (2 . b)) (s/> ht (s/ht cdr car) (s/sort-by car <))]
               )
    'ok))

(mat unfold ()
  (define (numbers-from n)
    (stream-unfold (lambda (n) (values n (+ n 1))) n))
  (define (countdown n)
    (stream-unfold
     (lambda (n)
       (if (zero? n)
           'liftoff
           (values n (- n 1))))
     3))
  (define (char-stream ip)
    (stream-unfold
     (lambda (ip)
       (if (port-eof? ip) (values) (values (get-char ip) ip)))
     ip))
  (match-let* ([(1 2 3) (s/> (numbers-from 1) (s/take 3))]
               [(4 5) (s/> (numbers-from 4) (s/take 2))]
               [(3 2 1 liftoff) (stream->list (countdown 3))]
               [(#\a #\b #\c) (stream->list (char-stream (open-string-input-port "abc")))]
               )
    'ok))

(mat s/do ()
  (define log '())
  (define result (s/> '(1 2) (s/do (lambda (x) (set! log (cons x log))))))
  (assert (eq? result (void)))
  (match-let* ([(2 1) log])
    'ok))

(mat stream-cons ()
  (match-let* ([(1) (stream->list (stream-cons 1 (stream)))]
               [(1 2) (stream->list (stream-cons 1 (stream 2)))]
               [(1 2 3) (stream->list (stream-cons 1 (stream 2 3)))]
               [() (stream->list (stream-cons* (stream)))]
               [(1) (stream->list (stream-cons* (stream 1)))]
               [(1) (stream->list (stream-cons* 1 (stream)))]
               [(1 2 3) (stream->list (stream-cons* 1 (stream 2 3)))]
               [(1 2 3 4) (stream->list (stream-cons* 1 2 (stream 3 4)))]
               )
    'ok))

(mat s/> ()
  (match-let* ([(1 2 3) (s/> '(1 2 3))]
               [(1 2 3) (s/> (stream 1 2 3))]
               ["1, 4, 9" (s/> '(1 2 3)
                            (s/map square)
                            (s/fold-right cons '())
                            (s/map number->string)
                            (lambda (s) (join (stream->list s) ", ")))]
               [`(catch #(invalid-stream 6)) (try (s/> '(1 2 3)
                                                    (s/fold-left + 0)
                                                    (s/map square)))]
               [6 (s/> '(1 2 3)
                    (s/fold-left + 0)
                    number->string
                    string->number)]
               [(1 4 9) (s/> '(1 2 3)
                          (s/map square)
                          stream->list
                          reverse
                          reverse)]
               [(2 3 4) (s/> '#(1 2 3) (s/map 1+))]
               [1 (s/> (cons 1 2) car)]
               )
    (assert (procedure? (s/> (stream 1) s/stream)))
    'ok))

(mat stream-lift ()
  (match-let* ([(3 2 1) (s/> '(1 2 3) (stream-lift reverse))]
               [(1 2 3) (unstream (stream 1 2 3))])
    'ok))

(mat stream-yield ()
  (define (fibs)
    (stream-yield
     (let lp ([a 0] [b 1])
       (yield a)
       (lp b (+ a b)))))

  (define (queens n)
    (define (attack? queens row col)
      (exists (lambda (q) (= (abs (- row (car q))) (abs (- col (cdr q))))) queens))
    (stream-yield
     (let next-row ([queens '()] [row 0] [cols (iota n)])
       (if (null? cols)
           (yield (reverse queens))
           (let next-col ([choices cols])
             (unless (null? choices)
               (let ([col (car choices)])
                 (when (not (attack? queens row col))
                   (next-row (cons (cons row col) queens) (+ row 1) (remq col cols)))
                 (next-col (cdr choices)))))))))

  (match-let* ([() (stream->list (stream-yield))]
               [(0 1 1 2 3 5 8 13) (s/> (fibs) (s/take 8))]
               [92 (s/> (queens 8) s/length)]
               )
    'ok))

(mat stream-search ()
  (define (basis-combinations ls)
    (if (<= (length ls) 2)
        (stream ls)
        (stream-search
         (lambda (flip _)
           (let generate ([ls ls] [require-head? #t])
             (let ([x (car ls)] [rest (cdr ls)])
               (cond
                [(null? rest) ls]
                [(or require-head? (flip))
                 (cons x (generate rest #f))]
                [else (generate rest #t)])))))))

  ;; returns a stream of lists of (row . col)
  (define (queens n)
    (define (attack? queens row col)
      (exists (lambda (q) (= (abs (- row (car q))) (abs (- col (cdr q))))) queens))
    (stream-search
     (lambda (flip fail)
       (let next-row ([queens '()] [row 0] [cols (iota n)])
         (if (null? cols)
             (reverse queens)
             (let next-col ([choices cols])
               (if (null? choices)
                   (fail)
                   (let ([col (car choices)])
                     (if (and (not (attack? queens row col)) (flip))
                         (next-row (cons (cons row col) queens) (+ row 1) (remq col cols))
                         (next-col (cdr choices)))))))))))

  ;; prints a (row . col) list
  #;
  (define (print-board qs)
    (let ([n (length qs)])
      (newline)
      (for-each
       (lambda (q)
         (do ([i 0 (+ i 1)]) [(= i n)]
           (if (= i (cdr q))
               (printf "Q ")
               (printf "· ")))
         (newline))
       qs)))

  (define (shortest-sum-paths n tree)
    (define (leaf? x)
      (null? (cdr x)))
    (alias value car)
    (alias children cdr)
    (stream-search
     (lambda (flip fail)
       (let next-level ([x tree] [sum (value tree)])
         (if (leaf? x)
             (if (= sum n) x (fail))
             (let next-child ([cs (children x)])
               (if (null? cs)
                   (fail)
                   (let ([c (car cs)])
                     (if (flip 'lifo)
                         (next-child (cdr cs))
                         (cons (value x) (next-level c (+ sum (value c)))))))))))))

  (define tree
    '(1
      (1
       (1
        (1
         (1))))
      (5
       (-1))
      (6)
      (1
       (2
        (1)))
      (4)))

  (match-let* ([() (stream->list (stream-search (lambda (_ fail) (fail))))]
               [(((0 1 2 3 4) (0 1 2 3 4 5 6 7 8 9))
                 ((0 1 2 4) (0 1 2 3 4 5 6 7 9))
                 ((0 1 3 4) (0 1 2 3 4 5 6 8 9))
                 ((0 2 3 4) (0 1 2 3 4 5 7 8 9)))
                (s/> (basis-combinations (iota 5))
                  (s/zip (basis-combinations (iota 10)) list)
                  (s/take 4))]
               [92 (s/> (queens 8) s/length)]
               [((1 4) (1 5 -1) (1 1 2 1) (1 1 1 1 1))
                (stream->list (shortest-sum-paths 5 tree))]
               [((1 6))
                (stream->list (shortest-sum-paths 7 tree))]
               [()
                (stream->list (shortest-sum-paths 8 tree))]
               )
    'ok))
