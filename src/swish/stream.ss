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
(library (swish stream)
  (export
   define-stream-transformer
   empty-stream
   eos
   eos?
   hashtable->stream
   list->stream
   require-stream
   s/>
   s/all?
   s/any
   s/any?
   s/by-key
   s/cells
   s/chunk
   s/chunk-every
   s/chunk2
   s/concat
   s/count
   s/do
   s/drop
   s/drop-last
   s/drop-while
   s/extrema
   s/extrema-by
   s/filter
   s/find
   s/first
   s/fold-left
   s/fold-right
   s/group-by
   s/ht
   s/ht*
   s/ht**
   s/join
   s/last
   s/length
   s/map
   s/map-car
   s/map-cdr
   s/map-concat
   s/map-cons
   s/map-extrema
   s/map-filter
   s/map-index
   s/map-max
   s/map-min
   s/map-uniq
   s/map-window
   s/max
   s/max-by
   s/mean
   s/min
   s/min-by
   s/none?
   s/reverse
   s/sort
   s/sort-by
   s/stream
   s/sum
   s/take
   s/take-last
   s/take-while
   s/uniq
   s/uniq-by
   s/zip
   stream
   stream->list
   stream->vector
   stream-cons
   stream-cons*
   stream-for-each
   stream-repeat
   stream-search-fifo
   stream-search-lifo
   stream-unfold
   stream-yield
   stream?
   stream-lift
   transformer-compose
   transformer-compose*
   unstream
   vector->stream
   )
  (import (chezscheme) (swish erlang))

  (define eos '#{eos mwqmpv0qkm5v30alavqzybvwc-0})
  (define (eos? x) (eq? x eos))
  (define empty-stream (lambda () eos))
  (define nil '#{nil l7rxffkwi4beedkbqv8sqmbyi-48})

  (define-record-type stream-box (opaque #t) (fields value))

  (define (stream . ls)
    (list->stream ls))

  (define (list->stream ls)
    (lambda ()
      (if (null? ls)
          eos
          (let ([v (car ls)])
            (set! ls (cdr ls))
            v))))

  (define (vector->stream v)
    (unless (vector? v) (bad-arg 'vector->stream v))
    (let ([i -1]
          [len (#3%vector-length v)])
      (lambda ()
        (set! i (#3%fx1+ i))
        (if (#3%fx< i len)
            (#3%vector-ref v i)
            eos))))

  (define (hashtable->stream ht)
    (vector->stream (hashtable-cells ht)))

  (define (s/cells x)
    (if (hashtable? x)
        (hashtable->stream x)
        x))

  (define (stream->list s)
    (let ([x (s)])
      (if (eos? x)
          '()
          (cons x (stream->list s)))))

  (define (stream->vector s)
    (define (build ls len)
      (let ([vec (make-vector len)])
        (let lp ([ls ls] [i len])
          (if (#3%fxzero? i)
              vec
              (let ([i (#3%fx1- i)])
                (#3%vector-set! vec i (car ls))
                (lp (cdr ls) i))))))
    (let lp ([ls '()] [len 0])
      (let ([x (s)])
        (if (eos? x)
            (build ls len)
            (lp (cons x ls) (#3%fx1+ len))))))

  (define (stream? x)
    (and (procedure? x) (= (procedure-arity-mask x) 1)))

  (define (stream-cons x s)
    (define (f)
      (set! f s)
      x)
    (lambda () (f)))

  (define (stream-cons* a . L)
    (define get
      (if (null? L)
          a
          (let ([args (cons a L)])
            (lambda ()
              (let ([h (car args)] [t (cdr args)])
                (if (null? (cdr t))
                    (set! get (car t))
                    (set! args t))
                h)))))
    (lambda () (get)))

  (define-syntax (stream-yield x)
    (syntax-case x ()
      [(k e ...)
       (with-implicit (k yield)
         #'(let ()
             (define return #f)
             (define (resume _)
               e ...
               (set! resume (lambda (_) eos))
               (return eos))
             (define (yield v) (mark/cc resume (return v)))
             (lambda () (mark/cc return (resume (void))))))]))

  (define-syntax mark/cc
    (syntax-rules ()
      [(_ id e ...)
       (call/1cc
        (lambda (k)
          (set! id k)
          e ...))]))

  (define (stream-for-each f s)
    (let lp ([x (s)])
      (unless (eos? x)
        (f x)
        (lp (s)))))

  (define (stream-unfold p state)
    (lambda ()
      (if (eq? state nil)
          eos
          (call-with-values
            (lambda () (p state))
            (case-lambda
             [()
              (set! state nil)
              eos]
             [(x)
              (set! state nil)
              x]
             [(x state*)
              (set! state state*)
              x])))))

  (define (stream-repeat n p)
    (unless (fixnum? n) (bad-arg 'stream-repeat n))
    (let ([i 0])
      (lambda ()
        (if (#3%fx< i n)
            (let ([x (p i)])
              (set! i (#3%fx1+ i))
              x)
            eos))))

  (define (->stream x)
    (cond
     [(stream? x) x]
     [(list? x) (list->stream x)]
     [(vector? x) (vector->stream x)]
     [(hashtable? x) (hashtable->stream x)]
     [else x]))

  (define (unstream v)
    (cond
     [(stream? v) (stream->list v)]
     [(stream-box? v) (stream-box-value v)]
     [else v]))

  (define (stream-lift p)
    (lambda (x)
      (p (unstream x))))

  (define (stream-search-lifo start)
    (define return #f)
    (define stack '())
    (define (flip)
      (call/cc
       (lambda (k)
         (set! stack (cons k stack))
         #t)))
    (define (fail)
      (if (null? stack)
          (return eos)
          (let ([k (car stack)])
            (set! stack (cdr stack))
            (k #f))))
    (define (go)
      (set! go fail)
      (return (start flip fail)))
    (lambda () (mark/cc return (go))))

  (define (stream-search-fifo start)
    (define return #f)
    (define queue (make-queue))
    (define (flip)
      (call/cc
       (lambda (k)
         (enqueue! queue k)
         #t)))
    (define (fail)
      (if (queue-empty? queue)
          (return eos)
          ((dequeue! queue) #f)))
    (define (go)
      (set! go fail)
      (return (start flip fail)))
    (lambda () (mark/cc return (go))))

  (define (make-queue)
    (cons '() #f))

  (define (enqueue! q x)
    (let ([h (car q)] [t (cdr q)] [p (list x)])
      (if t
          (begin
            (set-cdr! t p)
            (set-cdr! q p))
          (begin
            (set-car! q p)
            (set-cdr! q p)))))

  (define (dequeue! q)
    (let ([h (car q)] [t (cdr q)])
      (unless t (bad-arg 'dequeue! q))
      (let ([x (car h)] [h (cdr h)])
        (set-car! q h)
        (when (null? h)
          (set-cdr! q #f))
        x)))

  (define (queue-empty? q)
    (not (cdr q)))

  (define (require-stream x)
    (let ([x (->stream x)])
      (if (stream? x)
          x
          (throw `#(invalid-stream ,x)))))

  (define (transformer-compose a b)
    (lambda (s) (b (a s))))

  (define (transformer-compose* ts)
    (fold-left transformer-compose values ts))

  (define (s/> x . ts)
    (unstream ((transformer-compose* ts) (->stream x))))

  (define-syntax define-stream-transformer
    (syntax-rules ()
      [(_ (name s a1 a2 ...) e1 e2 ...)
       (define (name a1 a2 ...) ($stream-transformer (s a1 a2 ...) e1 e2 ...))]
      [(_ (name s) e1 e2 ...)
       (define name ($stream-transformer (s) e1 e2 ...))]))

  (define-syntax $stream-transformer
    (syntax-rules ()
      [(_ (s a ...) e1 e2 ...)
       (lambda (s)
         (let ([s (require-stream s)] [a a] ...)
           e1 e2 ...))]))

  (define-stream-transformer (s/fold-right s f acc)
    (let fold ([f f] [acc acc])
      (let ([x (s)])
        (if (eos? x)
            acc
            (f x (fold f acc))))))

  (define-stream-transformer (s/fold-left s f acc)
    (let fold ([f f] [acc acc])
      (let ([x (s)])
        (if (eos? x)
            acc
            (fold f (f acc x))))))

  (define-stream-transformer (s/map s f)
    (lambda ()
      (let ([x (s)])
        (if (eos? x)
            eos
            (f x)))))

  (define-stream-transformer (s/zip s s2 f)
    (let ([s2 (require-stream s2)])
      (lambda ()
        (let ([x (s)])
          (if (eos? x)
              eos
              (let ([x2 (s2)])
                (if (eos? x2)
                    (throw `#(stream-length-mismatch ,x))
                    (f x x2))))))))

  (define (s/map-index f)
    (s/zip (numbers-from 0) f))

  (define (numbers-from n)
    (stream-unfold (lambda (n) (values n (+ n 1))) n))

  (define-stream-transformer (s/concat s)
    (let ([head empty-stream])
      (lambda ()
        (let lp ()
          (let ([x (head)])
            (if (eos? x)
                (let ([head* (s)])
                  (if (eos? head*)
                      eos
                      (begin
                        (set! head (require-stream head*))
                        (lp))))
                x))))))

  (define (s/map-concat f)
    (transformer-compose (s/map f) s/concat))

  (define-syntax $filter
    (syntax-rules ()
      [(_ x fx)
       ($stream-transformer (s)
         (lambda ()
           (let lp ()
             (let ([x (s)])
               (cond
                [(eos? x) x]
                [fx x]
                [else (lp)])))))]))

  (define s/filter
    (case-lambda
     [() ($filter x x)]
     [(f) ($filter x (f x))]))

  (define (s/map-filter f)
    (transformer-compose (s/map f) (s/filter)))

  (define (s/map-car f)
    (s/map (lambda (p) (cons (f (car p)) (cdr p)))))

  (define (s/map-cdr f)
    (s/map (lambda (p) (cons (car p) (f (cdr p))))))

  (define (s/map-cons f)
    (s/map (lambda (x) (cons x (f x)))))

  (define-stream-transformer (s/take s n)
    (unless (fixnum? n) (bad-arg 's/take n))
    (if (#3%fx<= n 0)
        empty-stream
        (lambda ()
          (if (#3%fxzero? n)
              eos
              (let ([x (s)])
                (if (eos? x)
                    eos
                    (begin
                      (set! n (#3%fx1- n))
                      x)))))))

  (define (s/take-last n)
    (unless (fixnum? n) (bad-arg 's/take-last n))
    (cond
     [(#3%fx<= n 0) (lambda (_) empty-stream)]
     [(#3%fx= n 1) (lambda (s) (stream (s/last s)))]
     [else
      ;; assuming the number of values to take is small relative to the stream, it's just as fast to
      ;; double reverse as to use a sliding window.
      (transformer-compose* (list s/reverse (s/take n) s/reverse))]))

  (define-stream-transformer (s/drop s n)
    (unless (fixnum? n) (bad-arg 's/drop n))
    (if (#3%fx<= n 0)
        s
        (let lp ([n (#3%fx1- n)] [x (s)])
          (if (or (eos? x) (#3%fxzero? n))
              s
              (lp (#3%fx1- n) (s))))))

  (define-stream-transformer (s/drop-last s n)
    ;; assuming the number of values to drop is small relative to the stream, it's faster to use a
    ;; sliding window than to double reverse.
    (unless (fixnum? n) (bad-arg 's/drop-last n))
    (if (#3%fx<= n 0)
        s
        (let ([ls '()] [last '()] [len 0])
          (lambda ()
            (let lp ([x (s)])
              (if (eos? x)
                  eos
                  (call-with-values
                    (lambda () (window-add! ls last len n x))
                    (case-lambda
                     [() (lp (s))]
                     [(v) v]))))))))

  ;; Maintains a sliding window of at least one value. Once the window is full, adding a value to
  ;; the tail emits the head.
  (define-syntax window-add!
    (syntax-rules ()
      ;; ls: first pair or null
      ;; last: last pair or null
      ;; len: current length
      ;; max: window length
      ;; in: value to add
      [(_ ls last len max in)
       (cond
        [(null? ls)
         (let ([p (list in)])
           (set! ls p)
           (set! last p)
           (set! len 1)
           (values))]
        [(= len max)
         (let ([v (car ls)]
               [last* (list in)])
           (set-cdr! last last*)
           (set! last last*)
           (set! ls (cdr ls))
           (values v))]
        [else
         (let ([last* (list in)])
           (set-cdr! last last*)
           (set! last last*)
           (set! len (1+ len))
           (values))])]))

  (define-stream-transformer (s/take-while s f)
    (lambda ()
      (let ([x (s)])
        (if (or (eos? x) (not (f x)))
            eos
            x))))

  (define-stream-transformer (s/drop-while s f)
    (let lp ()
      (let ([x (s)])
        (cond
         [(eos? x) s]
         [(f x) (lp)]
         [else (stream-cons x s)]))))

  (define s/reverse
    (s/fold-left (lambda (ls x) (cons x ls)) '()))

  (define s/join
    (case-lambda
     [(sep) (s/join sep sep sep)]
     [(sep sep2 sep-last)
      ($stream-transformer (s)
        (let-values ([(op get) (open-string-output-port)])
          (let ([x1 (s)])
            (cond
             [(eos? x1) ""]
             [else
              (display x1 op)
              (let ([x2 (s)])
                (cond
                 [(eos? x2) (get)]
                 [else
                  (let ([x3 (s)])
                    (cond
                     [(eos? x3)
                      (display sep2 op)
                      (display x2 op)
                      (get)]
                     [else
                      (display sep op)
                      (display x2 op)
                      (let lp ([next x3])
                        (let ([x (s)])
                          (cond
                           [(eos? x)
                            (display sep-last op)
                            (display next op)
                            (get)]
                           [else
                            (display sep op)
                            (display next op)
                            (lp x)])))]))]))]))))]))

  (define-syntax $uniq
    (syntax-rules ()
      [(_ x fx $make-ht)
       (let ([make-ht $make-ht])
         ($stream-transformer (s)
           (let ([ht #f])
             (define (ensure-ht key)
               (set! ht (make-ht key))
               (set! ensure-ht (lambda (_) ht))
               ht)
             (lambda ()
               (let lp ()
                 (let ([x (s)])
                   (if (eos? x)
                       eos
                       (let* ([k fx]
                              [c (hashtable-cell (ensure-ht k) k #f)])
                         (if (#3%cdr c)
                             (lp)
                             (begin
                               (#3%set-cdr! c #t)
                               x))))))))))]))

  (define s/uniq
    (case-lambda
     [() ($uniq x x make-hashtable-for-key)]
     [(hash equiv?) ($uniq x x (lambda (_) (make-hashtable hash equiv?)))]))

  (define s/uniq-by
    (case-lambda
     [(f) ($uniq x (f x) make-hashtable-for-key)]
     [(f hash equiv?) ($uniq x (f x) (lambda (_) (make-hashtable hash equiv?)))]))

  (define s/map-uniq
    (case-lambda
     [(f) (transformer-compose (s/map f) (s/uniq))]
     [(f hash equiv?) (transformer-compose (s/map f) (s/uniq hash equiv?))]))

  (define-syntax $ht
    (syntax-rules ()
      [(_ x fkx fvx $make-ht)
       (let ([make-ht $make-ht])
         ($stream-transformer (s)
           (let ([ht #f])
             (define (ensure-ht key)
               (set! ht (make-ht key))
               (set! ensure-ht (lambda (_) ht))
               ht)
             (let lp ()
               (let ([x (s)])
                 (if (eos? x)
                     (ensure-ht #f)
                     (let* ([k fkx]
                            [c (hashtable-cell (ensure-ht k) k nil)])
                       (let ([v fvx])
                         (if (eq? (#3%cdr c) nil)
                             (begin
                               (#3%set-cdr! c v)
                               (lp))
                             (throw `#(duplicate-key ,k ,(cdr c) ,v)))))))))))]))

  (define s/by-key
    (case-lambda
     [(f) ($ht x (f x) x make-hashtable-for-key)]
     [(f hash equiv?) ($ht x (f x) x (lambda (_) (make-hashtable hash equiv?)))]))

  (define s/ht
    (case-lambda
     [(fk fv) ($ht x (fk x) (fv x) make-hashtable-for-key)]
     [(fk fv hash equiv?) ($ht x (fk x) (fv x) (lambda (_) (make-hashtable hash equiv?)))]))

  (define-syntax $ht*
    (syntax-rules ()
      [(_ x fkx fvx $make-ht $fix-value)
       (let ([make-ht $make-ht]
             [fix-value $fix-value])
         ($stream-transformer (s)
           (let ([ht #f])
             (define (ensure-ht key)
               (set! ht (make-ht key))
               (set! ensure-ht (lambda (_) ht))
               ht)
             (let lp ()
               (let ([x (s)])
                 (if (eos? x)
                     (begin
                       (vector-for-each (lambda (c) (set-cdr! c (fix-value (cdr c))))
                         (hashtable-cells (ensure-ht #f)))
                       ht)
                     (let* ([k fkx]
                            [c (hashtable-cell (ensure-ht k) k '())])
                       (#3%set-cdr! c (cons fvx (#3%cdr c)))
                       (lp))))))))]))

  (define s/group-by
    (case-lambda
     [(f) ($ht* x (f x) x make-hashtable-for-key reverse)]
     [(f hash equiv?) ($ht* x (f x) x (lambda (_) (make-hashtable hash equiv?)) reverse)]))

  (define s/ht*
    (case-lambda
     [(fk fv) ($ht* x (fk x) (fv x) make-hashtable-for-key reverse)]
     [(fk fv hash equiv?) ($ht* x (fk x) (fv x) (lambda (_) (make-hashtable hash equiv?)) reverse)]))

  (define s/ht**
    (let ()
      (define (fix v)
        (fold-left (lambda (tail ls) (append ls tail)) '() v))
      (case-lambda
       [(fk fv) ($ht* x (fk x) (fv x) make-hashtable-for-key fix)]
       [(fk fv hash equiv?) ($ht* x (fk x) (fv x) (lambda (_) (make-hashtable hash equiv?)) fix)])))

  (define (make-hashtable-for-key k)
    (cond
     [(or (fixnum? k) (record? k) (boolean? k)) (make-eq-hashtable)]
     [(or (number? k) (char? k)) (make-eqv-hashtable)]
     [(string? k) (make-hashtable string-hash string=?)]
     [(symbol? k) (make-hashtable symbol-hash eq?)]
     [else (make-hashtable equal-hash equal?)]))

  (define (s/chunk-every n)
    (unless (and (fixnum? n) (positive? n)) (bad-arg 's/chunk-every n))
    ($stream-transformer (s n)
      (lambda ()
        (let lp ([c '()] [r n])
          (let ([x (s)])
            (cond
             [(eos? x)
              (if (null? c)
                  eos
                  (reverse c))]
             [(#3%fx= r 1)
              (reverse (cons x c))]
             [else
              (lp (cons x c) (#3%fx1- r))]))))))

  (define-stream-transformer (s/chunk s f)
    (let ([x (s)])
      (if (eos? x)
          empty-stream
          (let ([c* (list x)] [q* (f x)])
            (lambda ()
              (if (not c*)
                  eos
                  (let lp ([c c*])
                    (let ([x (s)])
                      (if (eos? x)
                          (begin
                            (set! c* #f)
                            (reverse c))
                          (let ([q (f x)])
                            (if (equal? q q*)
                                (lp (cons x c))
                                (begin
                                  (set! c* (list x))
                                  (set! q* q)
                                  (reverse c)))))))))))))

  (define-stream-transformer (s/chunk2 s f)
    (let ([x1 (s)])
      (if (eos? x1)
          empty-stream
          (let ([x2 (s)])
            (if (eos? x2)
                (stream (list x1))
                (let ([c* (list x2 x1)] [q* (f x1 x2)])
                  (lambda ()
                    (if (not c*)
                        eos
                        (let lp ([c c*] [last-x (car c*)])
                          (let ([x (s)])
                            (if (eos? x)
                                (begin
                                  (set! c* #f)
                                  (reverse c))
                                (let ([q (f last-x x)])
                                  (if (equal? q q*)
                                      (lp (cons x c) x)
                                      (begin
                                        (set! c* (list x))
                                        (set! q* q)
                                        (reverse c)))))))))))))))

  (define-stream-transformer (s/first s)
    (let ([x (s)])
      (and (not (eos? x)) x)))

  (define-stream-transformer (s/last s)
    (let ([x (s)])
      (and (not (eos? x))
           (let lp ([prev-x x])
             (let ([x (s)])
               (if (eos? x)
                   prev-x
                   (lp x)))))))

  (define-stream-transformer (s/length s)
    (let lp ([n 0])
      (if (eos? (s))
          n
          (lp (#3%fx1+ n)))))

  (define s/mean
    (case-lambda
     [()
      ($stream-transformer (s)
        (let lp ([n 0] [sum 0])
          (let ([x (s)])
            (if (eos? x)
                (if (#3%fxzero? n)
                    +nan.0
                    (/ sum n))
                (lp (#3%fx1+ n) (+ sum x))))))]
     [(f) (transformer-compose (s/map f) (s/mean))]))

  (define-stream-transformer (s/any s)
    (let ([x (s)])
      (if (eos? x)
          #f
          (stream-cons x s))))

  (define s/any?
    (case-lambda
     [() ($stream-transformer (s) (not (eos? (s))))]
     [(f) (transformer-compose (s/filter f) (s/any?))]))

  (define s/none?
    (case-lambda
     [() ($stream-transformer (s) (not ((s/any?) s)))]
     [(f) ($stream-transformer (s) (not ((s/any? f) s)))]))

  (define-stream-transformer (s/all? s f)
    (let lp ()
      (let ([x (s)])
        (cond
         [(eos? x) #t]
         [(f x) (lp)]
         [else #f]))))

  (define (s/stream x)
    (make-stream-box x))

  (define-stream-transformer (s/sum s)
    (let lp ([sum 0])
      (let ([x (s)])
        (if (eos? x)
            sum
            (lp (+ sum x))))))

  (define-stream-transformer (s/count s f)
    (let lp ([c 0])
      (let ([x (s)])
        (if (eos? x)
            c
            (let ([v (f x)])
              (cond
               [(not v) (lp c)]
               [(number? v) (lp (+ c v))]
               [else (lp (+ c 1))]))))))

  (define-syntax $extremum
    (syntax-rules ()
      [(_ x fx op)
       ($stream-transformer (s)
         (let ([x (s)])
           (and (not (eos? x))
                (let lp ([mx x] [mv fx])
                  (let ([x (s)])
                    (if (eos? x)
                        mx
                        (let ([v fx])
                          (if (op v mv)
                              (lp x v)
                              (lp mx mv)))))))))]))

  (define-syntax $extrema
    (syntax-rules ()
      [(_ x fx lt gt)
       ($stream-transformer (s)
         (let ([x (s)])
           (and (not (eos? x))
                (let ([v fx])
                  (let lp ([minx x] [minv v] [maxx x] [maxv v])
                    (let ([x (s)])
                      (if (eos? x)
                          (list minx maxx)
                          (let ([v fx])
                            (cond
                             [(lt v minv) (lp x v maxx maxv)]
                             [(gt v maxv) (lp minx minv x v)]
                             [else (lp minx minv maxx maxv)])))))))))]))

  (define s/min
    (case-lambda
     [() ($extremum x x <)]
     [(lt) ($extremum x x lt)]))

  (define s/max
    (case-lambda
     [() ($extremum x x >)]
     [(gt) ($extremum x x gt)]))

  (define s/min-by
    (case-lambda
     [(f) ($extremum x (f x) <)]
     [(f lt) ($extremum x (f x) lt)]))

  (define s/max-by
    (case-lambda
     [(f) ($extremum x (f x) >)]
     [(f gt) ($extremum x (f x) gt)]))

  (define s/extrema
    (case-lambda
     [() ($extrema x x < >)]
     [(lt gt) ($extrema x x lt gt)]))

  (define s/extrema-by
    (case-lambda
     [(f) ($extrema x (f x) < >)]
     [(f lt gt) ($extrema x (f x) lt gt)]))

  (define s/map-min
    (case-lambda
     [(f) (transformer-compose (s/map f) (s/min))]
     [(f lt) (transformer-compose (s/map f) (s/min lt))]))

  (define s/map-max
    (case-lambda
     [(f) (transformer-compose (s/map f) (s/max))]
     [(f gt) (transformer-compose (s/map f) (s/max gt))]))

  (define s/map-extrema
    (case-lambda
     [(f) (transformer-compose (s/map f) (s/extrema))]
     [(f lt gt) (transformer-compose (s/map f) (s/extrema lt gt))]))

  (define (s/map-window n f)
    (unless (and (fixnum? n) (positive? n)) (bad-arg 's/map-window n))
    ($stream-transformer (s n f)
      (let ([ls '()] [last '()] [len 0])
        (lambda ()
          (let lp ([x (s)])
            (if (eos? x)
                eos
                (call-with-values
                  (lambda () (window-add! ls last len n x))
                  (case-lambda
                   [()
                    (if (#3%fx= len n)
                        (f (fold-right cons '() ls))
                        (lp (s)))]
                   [(v)
                    (f (fold-right cons '() ls))]))))))))

  (define (s/find f)
    (transformer-compose (s/filter f) s/first))

  (define-stream-transformer (s/do s f)
    (let lp ()
      (let ([x (s)])
        (unless (eos? x)
          (f x)
          (lp)))))

  (define (s/sort lt)
    (lambda (x)
      (cond
       [(stream? x) (sort lt (stream->list x))]
       [(list? x) (sort lt x)]
       [(vector? x) (vector->list (vector-sort lt x))]
       [(hashtable? x) (vector->list (vector-sort lt (hashtable-cells x)))]
       [else (throw `#(invalid-stream ,x))])))

  (define (s/sort-by f lt)
    (s/sort (lambda (a b) (lt (f a) (f b)))))
  )
