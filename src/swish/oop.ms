;;; Copyright 2025 Indigo BioAutomation, Inc.
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
 (swish oop)
 (swish testing)
 )

(mat syntax-errors ()
  (assert-syntax-error base "invalid context for")
  (assert-syntax-error this "invalid context for")
  (assert-syntax-error (define-class A (parent A)) "unrecognized parent class")
  (assert-syntax-error (lambda () (define-class A) (define-class B (parent A) (parent A)))
    "duplicate definition")
  (assert-syntax-error (define-class A (fields) (fields)) "duplicate definition")
  (assert-syntax-error (define-class A (fields x)) "invalid syntax")
  (assert-syntax-error (define-class A (fields (immutable #f))) "invalid syntax")
  (assert-syntax-error (define-class A (fields (immutable x) (boolean y))) "invalid syntax")
  (assert-syntax-error (define-class A (fields (mutable x) (immutable x))) "duplicate definition")
  (assert-syntax-error (define-class A (protocol #f) (protocol #f)) "duplicate definition")
  (assert-syntax-error (define-class A (fields (immutable x)) (method (x))) "duplicate definition")
  (assert-syntax-error (define-class A (method (isa?))) "reserved name")
  (assert-syntax-error (define-class A (method (x)) (virtual (x))) "duplicate definition")
  (assert-syntax-error (define-class A (fields (immutable x)) (method (m) (set! x 0)))
    "invalid syntax")
  (assert-syntax-error (lambda () (define-class A) (A x (A make))) "unknown class A member")
  (assert-syntax-error (define-class A (method (m) (this make))) "unknown class A member")
  (assert-syntax-error (define-class A (method (m) (this x))) "unknown class A member")
  (assert-syntax-error (define-class A (fields (immutable x)) (method (m) (base x)))
    "no parent for")
  (assert-syntax-error (define-class A (fields (immutable x)) (method (m) (this x 0)))
    "incorrect number of arguments to class A field")
  (assert-syntax-error (lambda ()
                         (define-class A (fields (mutable x)))
                         (define-class B (parent A) (fields (immutable x)))
                         (A x (B make 0 1) 2)
                         (B x (B make 0 1) 2))
    "incorrect number of arguments to class B field")
  (assert-syntax-error (lambda ()
                         (define-class A (fields (mutable x)))
                         (define-class B (parent A) (method (m) (base x 1 2)))
                         0)
    "incorrect number of arguments to class A field")
  (assert-syntax-error (lambda ()
                         (define-class A (method (m)))
                         (A m (A make) 1))
    "incorrect number of arguments to class A instance method")
  (assert-syntax-error (lambda ()
                         (define-class A)
                         (define-class B (parent A) (method (m) (base make)))
                         0)
    "unknown class A member")
  (assert-syntax-error (lambda ()
                         (define-class A)
                         (define-class B (parent A) (method (m) (base x)))
                         0)
    "unknown class A member")
  (assert-syntax-error (lambda ()
                         (define-class A)
                         (A isa?))
    "incorrect number of arguments to")
  )

(mat one-class ()
  (define-class A
    (fields (immutable f1) (mutable f2))
    (method (m1) f1)
    (method (m2) f2)
    (method (me) this)
    (virtual (m1 f1) `(A ,f1))
    (virtual (m1 f1 f2) (this f2 f1) `(A ,(this f1) ,(this f2)))
    (virtual (m2 m1) (list m1 (this m1) (m2))))
  (match-let*
   ([,a (A make 1 2)]
    ["#[A 1 2]" (parameterize ([print-gensym #f]) (format "~a" a))]
    [#t (A isa? a)]
    [,@a (A me a)]
    ["Exception in me: #f is not of type #<record type A>."
     (exit-reason->english (catch (A me #f)))]
    [1 (A f1 a)]
    [1 (A m1 a)]
    [2 (A f2 a)]
    [2 (A m2 a)]
    [(A 12) (A m1 a 12)]
    [(A 1 3) (A m1 a 3 0)]
    [3 (A m2 a)]
    [(12 1 3) (A m2 a 12)]
    )
   'ok))

(mat hierarchy ()
  (define-class A
    (fields (immutable f1) (mutable f2))
    (method (m1) `(A.m1))
    (method (m2) `(A.m2))
    (virtual (m1 f1) `(A.m1 ,f1))
    (virtual (m1 f1 f2) `(A.m1 ,f1 ,f2)))
  (define-class B
    (parent A)
    (fields (immutable f2) (mutable m2))
    (method (m1) `(B.m1))
    (method (m4 x) (base f2 x) (base f2))
    (method (m5) (base m2))
    (virtual (m1 m3) `(B.m1 ,(base m1 m3)))
    (virtual (m3) (base m1)))
  (define-class C
    (parent B)
    (method (m2 x y) (+ x y))
    (virtual (f1 x y) (- x y))
    (method (f1) 'C-f1)
    (method (g) (list (f1) (base f1))))
  (match-let*
   ([,a (A make 1 2)]
    [1 (A f1 a)]
    [2 (A f2 a)]
    [-2 (begin (A f2 a -2) (A f2 a))]
    [(A.m1) (A m1 a)]
    [(A.m2) (A m2 a)]
    [(A.m1 foo) (A m1 a 'foo)]
    [(A.m1 foo bar) (A m1 a 'foo 'bar)]
    [,b (B make 1 2 3 4)]
    [1 (A f1 b)]
    [2 (A f2 b)]
    [#t (A isa? a)]
    [#t (A isa? b)]
    [#f (B isa? a)]
    [#t (B isa? b)]
    [1 (B f1 b)]
    [3 (B f2 b)]
    ;; field shadows inherited method
    [4 (B m2 b)]
    [(A.m1) (A m1 b)]
    [(B.m1) (B m1 b)]
    [(A.m1) (B m3 b)]
    [bar (B m4 b 'bar)]
    [(A.m2) (B m5 b)]
    [(B.m1 (A.m1 foo)) (B m1 b 'foo)]
    [(A.m1) (B m3 b)]
    [,c (C make 1 2 3 4)]
    [13 (C m2 c 10 3)]
    ;; method f1 shadows inherited field f1
    [(C-f1 1) (C g c)]
    [7 (C f1 c 10 3)])
   'ok)
  (match-let*
   ([,a (A make 3 4)]
    [,b (B make 5 6 7 8)]
    [,b-alt
     (let ()
       (define-class B (fields (immutable x)))
       (B make 'still))]
    [,c-alt
     (let ()
       ;; like-named subclass in a different scope
       (define-class C (parent B) (virtual (m1 z) (list 'z= z (base m1 z))))
       (C make 10 9 8 7))]
    [(z= 3 (B.m1 (A.m1 3))) (B m1 c-alt 3)]
    ["Exception in m1: #[A 3 4] is not of type #<record type B>."
     (parameterize ([print-gensym #f])
       (exit-reason->english (try (B m1 a))))]
    ["Exception in m5: #[B still] is not of type #<record type B>."
     (parameterize ([print-gensym #f])
       (exit-reason->english (try (B m5 b-alt))))]
    ["Exception in g: #[B 5 6 7 8] is not of type #<record type C>."
     (parameterize ([print-gensym #f])
       (exit-reason->english (try (C g b))))])
   'ok)
  (let ()
    ;; recursive call to virtual goes through virtual table
    (define-class A (virtual (m x) (if (zero? x) 1 (* x (m (- x 1))))))
    (define-class B (parent A) (virtual (m x) 7) (method (m2 x) (base m x)))
    (match-let*
     ([,a (A make)]
      [,b (B make)]
      [120 (A m a 5)]
      [7 (B m b 5)]
      [35 (B m2 b 5)])
     'ok))
  (let ()
    ;; formals shadow inherited fields
    (define-class A (fields (immutable x)))
    (define-class B (parent A) (method (m x) x))
    (match-let*
     ([,b (B make 123)]
      [17 (B m b 17)])
     'ok))
  )

(mat name-shadowing ()
  (define-class A
    (fields (mutable x) (mutable set!))
    (method (dump) (list set! x)))
  (define-class B (parent A)
    (fields (immutable lambda))
    (protocol (lambda (base) (lambda (x set!) ((base x set!) 'lambda))))
    (method (m0) (set!))
    (method (m1) (set! (set!)))
    (method (m2 y) (set! x y))
    (method (m3) (let ([r lambda]) (import (scheme)) ((lambda () r)))))
  (define f
    (case-lambda
     [() (list 'zero)]
     [(a) (list 'one a)]
     [(a b) (list 'two a b)]))
  (match-let*
   ([,b (B make 'b1-x f)]
    [,expected (A dump b)]
    [(,@f b1-x) expected]
    [(zero) (B m0 b)]
    [,@expected (A dump b)]
    [(one (zero)) (B m1 b)]
    [,@expected (A dump b)]
    [(two b1-x 123) (B m2 b 123)]
    [lambda (B m3 b)])
   'ok)
  )

(mat protocol ()
  (define-class A
    (fields (immutable f1) (immutable f2))
    (protocol
     (lambda (base)
       (lambda ()
         ((base) 1 2)))))
  (define-class B
    (parent A)
    (fields (immutable f3) (immutable f4))
    (protocol
     (lambda (base)
       (lambda (x)
         ((base) x x)))))
  (define-class C
    (parent B)
    (fields (immutable f3) (immutable f4))
    (protocol
     (lambda (base)
       (lambda (f3 f4)
         ((base 3) f3 f4)))))
  (match-let*
   ([,a (A make)]
    [1 (A f1 a)]
    [2 (A f2 a)]
    [,b (B make 3)]
    [1 (B f1 b)]
    [2 (B f2 b)]
    [3 (B f3 b)]
    [3 (B f4 b)]
    [,c (C make 5 6)]
    [1 (C f1 c)]
    [2 (C f2 c)]
    [3 (B f3 c)]
    [3 (B f4 c)]
    [5 (C f3 c)]
    [6 (C f4 c)])
   'ok))

(mat compilation ()
  (define-values (op1 get-bytevector1) (open-bytevector-output-port))
  (define-values (op2 get-bytevector2) (open-bytevector-output-port))
  (compile-to-port
   '((define-class A (fields (immutable x)))
     (define-class B (parent A) (fields (immutable y))))
   op1)
  (compile-to-port
   '((define-class C (parent B) (fields (immutable x)))
     (define (f obj)
       (list (B x obj) (B y obj)
         (and (C isa? obj) (C x obj))))
     (cons (B make 1 2) (C make 3 4 5)))
   op2)
  (match-let*
   ([,_
     (load-compiled-from-port
      (open-bytevector-input-port (get-bytevector1)))]
    [(,b . ,c)
     (load-compiled-from-port
      (open-bytevector-input-port (get-bytevector2)))]
    [(1 2 #f) (f b)]
    [(3 4 5) (f c)])
   'ok))
