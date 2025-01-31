;;; Copyright 2017 Beckman Coulter, Inc.
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

(library (swish testing)
  (export
   assert-syntax-error
   delete-tree
   gc
   incorrect-argument-count?
   match-prefix
   match-regexps
   output-dir
   sleep-ms
   write-test-file
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   (swish json)
   (swish mat)
   (swish meta)
   (swish pregexp)
   (swish string-utils)
   )

  (define-syntax assert-syntax-error
    (syntax-rules ()
      [(_ e expected) ($assert-syntax-error 'e expected #f)]
      [(_ e expected match-form) ($assert-syntax-error 'e expected match-form)]))

  (define ($assert-syntax-error e expected match-form)
    (define (matches? msg)
      (cond
       [(string? expected) (string=? msg expected)]
       [(procedure? expected) (expected msg)]
       [(list? expected) (pregexp-match expected msg)]))
    (guard
     (x
      [(and (syntax-violation? x) (matches? (condition-message x)))
       (when match-form
         (match-form e (syntax-violation-form x)))
       'ok])
     (eval e)
     (errorf 'assert-syntax-error "failed to raise syntax error: ~s" e)))

  (define (delete-tree path)
    (if (file-directory? path)
        (or (delete-directory path)
            (begin
              (for-each (lambda (p) (delete-tree (path-combine path p)))
                (directory-list path))
              (delete-directory path)))
        (delete-file path)))

  (define (gc)
    (debug-condition #f) ;; in case we've stashed a continuation condition
    (collect (collect-maximum-generation))
    (sleep-ms 10))

  (define arg-count-err-msg
    (let ([f (lambda (x) x)])
      (guard (c [else (condition-message c)])
        ((eval '(lambda (f) (f 1 2 3))) f))))

  (define (incorrect-argument-count? err proc)
    (define (procedure-name x)
      (let ([insp (inspect/object x)])
        (and (eq? (insp 'type) 'procedure)
             (string->symbol ((insp 'code) 'name)))))
    (define reason
      (match err
        [`(catch ,reason ,_) reason]
        [,_ err]))
    (match (condition-irritants reason)
      [(,_arg-count ,@proc) 'ok]
      [(,_arg-count ,x)
       (guard (eq? proc (procedure-name x)))
       'ok])
    (match-let*
     ([,@arg-count-err-msg (condition-message reason)])
     #t))

  (define (match-prefix lines pattern)
    (match lines
      [() (throw `#(pattern-not-found ,pattern))]
      [(,line . ,rest)
       (if (starts-with? line pattern)
           line
           (match-prefix rest pattern))]))

  (define (match-regexps patterns ls)
    (let check ([patterns patterns] [remaining-lines ls])
      (match patterns
        [() remaining-lines]
        [(seek ,pattern . ,patterns)
         (let search ([re (pregexp pattern)] [lines remaining-lines])
           (match lines
             [() (throw `#(pattern-not-found seek ,pattern ,remaining-lines))]
             [(,line . ,lines)
              (if (pregexp-match re line)
                  (check patterns lines)
                  (search re lines))]))]
        [(,pattern . ,patterns)
         (match remaining-lines
           [(,line . ,lines)
            (guard (pregexp-match pattern line))
            (check patterns lines)]
           [,_ (throw `#(pattern-not-found ,pattern ,remaining-lines))])])))

  (define (output-dir)
    (path-combine "tmp" "mat-output"))

  (define (sleep-ms t)
    (let* ([s (div t 1000)]
           [ns (* (- t (* s 1000)) 1000000)])
      (sleep (make-time 'time-duration ns s))))

  (define (write-test-file filename thunk)
    (let* ([full-name (path-combine (output-dir) filename)]
           [op (open-output-file (make-directory-path full-name) 'replace)])
      (on-exit (close-port op)
        (parameterize ([current-output-port op])
          (thunk)))
      full-name))
  )
