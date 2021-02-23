;;; Copyright 2018 Beckman Coulter, Inc.
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
(library (swish errors)
  (export
   current-exit-reason->english
   exit-reason->english
   exit-reason->stacks
   swish-exit-reason->english
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish internal)
   )

  ($import-internal &fault-condition)

  (define (exit-reason->english x)
    ((current-exit-reason->english) x))

  (define (swish-exit-reason->english x)
    (match x
      [#(EXIT ,x) (exit-reason->english x)]
      [#(bad-arg ,who ,arg) (format "Invalid argument to ~a: ~s." who arg)]
      [#(bad-match ,v ,src) (format "Pattern match failed~a: ~s." (src->english src) v)]
      [#(bad-tuple ,name ,x ,src) (format "Invalid type for tuple ~a~a: ~s." name (src->english src) x)]
      [#(invalid-datum ,x) (format "Invalid datum: ~s." x)]
      [#(unexpected-input ,x ,position) (format "Unexpected input at position ~d: ~s." position x)]
      [invalid-surrogate-pair "Invalid Unicode surrogate pair"]
      [unexpected-eof "Unexpected end-of-file."]
      [`(&fault-condition ,reason) (exit-reason->english reason)]

      ;; The following must come last:
      [,x
       (cond
        [(string? x) x]
        [(condition? x)
         (let ([op (open-output-string)])
           (display-condition x op)
           (write-char #\. op)
           (get-output-string op))]
        [else (format "~s" x)])]))

  (define current-exit-reason->english
    (make-parameter swish-exit-reason->english))

  (define (src->english x)
    (match x
      [#(,at ,offset ,file) (format " ~a offset ~a of ~a" at offset file)]
      [,_ ""]))

  (define (exit-reason->stacks reason)
    (define (get-k r)
      (if (continuation-condition? r)
          (condition-continuation r)
          (match r
            [#(EXIT ,reason) (get-k reason)]
            [,_ #f])))
    (define (cons-k r k*)
      (let ([k (get-k r)])
        (if (#%$continuation? k) (cons k k*) k*)))
    (define (add-stack k* reason)
      (match reason
        [`(&fault-condition [reason ,r] ,inner*)
         (fold-left add-stack (cons-k reason (cons-k r k*)) inner*)]
        [,_ (cons-k reason k*)]))
    (add-stack '() reason))

  (define-syntax redefine
    (syntax-rules ()
      [(_ var e) (#%$set-top-level-value! 'var e)]))

  ;; Native debugger doesn't know how to print our fault-condition and doesn't
  ;; understand multiple stacks, so package up a message condition and shadow
  ;; c's continuation by picking the first k returned by exit-reason->english,
  ;; which is typically closest to the source of the original error. Folks get
  ;; reasonable default behavior and they can inspect the condition directly
  ;; for more details.
  (redefine debug-condition
    (let ([system-debug-condition (#%$top-level-value 'debug-condition)])
      (case-lambda
       [() (system-debug-condition)]
       [(c)
        (system-debug-condition
         (match c
           [`(&fault-condition)
            (parameterize ([print-graph #t])
              (let ([msg (make-message-condition (exit-reason->english c))])
                (match (exit-reason->stacks c)
                  [(,k0 ,k1 . ,_)
                   (condition (make-continuation-condition k0) c msg)]
                  [,_ (condition c msg)])))]
           [,_ c]))])))

  )
