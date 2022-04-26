;;; Copyright 2022 Indigo BioAutomation, Inc.
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
(library (swish heap)
  (export
   heap-delete-min!
   heap-insert!
   heap-min
   heap-min-size
   heap-size
   heap-value<?
   heap?
   make-heap
   )
  (import (chezscheme) (swish erlang) (swish meta))

  (define-record-type heap
    (nongenerative)
    (sealed #t)
    (fields
     (mutable size)
     (mutable array)
     (immutable value<?)
     (immutable min-size))
    ;; array[0] is the minimum element
    ;; array[i] has children array[2*i+1] and array[2*i+2]
    ;; array[i] has parent array[(i-1) div 2]
    (protocol
     (lambda (new)
       (rec make-heap
         (case-lambda
          [(value<? min-size)
           (arg-check 'make-heap
             [value<? procedure?]
             [min-size fixnum? (lambda (x) (#3%fx> x 2))])
           (new 0 (make-vector min-size #f) value<? min-size)]
          [(value<?) (make-heap value<? 3)])))))

  (define (heap-min h)
    (when (#3%fx= (heap-size h) 0)
      (throw 'heap-empty))
    (#3%vector-ref (heap-array h) 0))

  (define (heap-insert! h x)
    (let ([size (heap-size h)])
      ;; double when full and add 1 to make vector-length odd (eliminates wasted word)
      (when (#3%fx= size (#3%vector-length (heap-array h)))
        (resize! h (#3%fx+ (#3%fx* size 2) 1)))
      (heap-size-set! h (#3%fx+ size 1))
      (upheap! size (heap-array h) x (heap-value<? h))))

  (define (heap-delete-min! h)
    (let ([size (heap-size h)])
      (when (#3%fx= size 0)
        (throw 'heap-empty))
      (let* ([a (heap-array h)]
             [capacity (#3%vector-length a)]
             [min (#3%vector-ref a 0)]
             [new-size (#3%fx- size 1)]
             [last (#3%vector-ref a new-size)])
        (downheap! 0 a last new-size (heap-value<? h))
        (#3%vector-set! a new-size #f)
        (heap-size-set! h new-size)
        ;; halve when quarter full
        (when (and (#3%fx<= (#3%fx+ (#3%fx* new-size 4) 3) capacity)
                   (#3%fx< (heap-min-size h) capacity))
          (resize! h (#3%fxdiv (#3%fx- capacity 1) 2)))
        min)))

  (define (upheap! i a x value<?)
    (if (#3%fx= i 0)
        (#3%vector-set! a 0 x)
        (let* ([j (#3%fxdiv (#3%fx- i 1) 2)]
               [p (#3%vector-ref a j)])
          (if (value<? x p)
              (begin
                (#3%vector-set! a i p)
                (upheap! j a x value<?))
              (#3%vector-set! a i x)))))

  (define (downheap! i a x last value<?)
    (let ([l (#3%fx+ (#3%fx* i 2) 1)])
      (if (#3%fx<= l last)
          (let ([left (#3%vector-ref a l)]
                [right (and (#3%fx< l last) (#3%vector-ref a (#3%fx+ l 1)))])
            (if (value<? left x)
                (if (and right (value<? right left))
                    (begin
                      (#3%vector-set! a i right)
                      (downheap! (#3%fx+ l 1) a x last value<?))
                    (begin
                      (#3%vector-set! a i left)
                      (downheap! l a x last value<?)))
                (if (and right (value<? right x))
                    (begin
                      (#3%vector-set! a i right)
                      (downheap! (#3%fx+ l 1) a x last value<?))
                    (#3%vector-set! a i x))))
          (#3%vector-set! a i x))))

  (define (resize! h new-size)
    (let ([a (heap-array h)]
          [new-a (make-vector new-size #f)]
          [size (heap-size h)])
      (do ([i 0 (#3%fx+ i 1)])
          ((#3%fx= i size) (heap-array-set! h new-a))
        (#3%vector-set! new-a i (#3%vector-ref a i))))))
