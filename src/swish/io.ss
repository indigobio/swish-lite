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
(library (swish io)
  (export
   binary->utf8
   make-directory-path
   make-utf8-transcoder
   path-absolute
   path-combine
   path-normalize
   )
  (import
   (chezscheme)
   )

  (define (binary->utf8 bp)
    (transcoded-port bp (make-utf8-transcoder)))

  (define make-directory-path
    (case-lambda
     [(path mode)
      (let loop ([path path])
        (let ([dir (path-parent path)])
          (unless (or (string=? dir path) (string=? dir ""))
            (unless (file-directory? dir)
              (loop dir)
              (mkdir dir mode)))))
      path]
     [(path) (make-directory-path path #o777)]))

  (define (make-utf8-transcoder)
    (make-transcoder (utf-8-codec)
      (eol-style none)
      (error-handling-mode replace)))

  (define path-absolute
    (case-lambda
     [(path) (path-absolute path (current-directory))]
     [(path base)
      (path-normalize
       (cond
        [(path-absolute? path) path]
        [(path-absolute? base) (path-combine base path)]
        [else (path-combine (current-directory) base path)]))]))

  (define path-combine
    (case-lambda
     [(x y)
      (let ([n (string-length x)])
        (cond
         [(eqv? n 0) y]
         [(directory-separator? (string-ref x (fx- n 1)))
          (string-append x y)]
         [else (format "~a~c~a" x (directory-separator) y)]))]
     [(x) x]
     [(x y . rest) (apply path-combine (path-combine x y) rest)]))

  (define (path-normalize path)
    (define abs? (path-absolute? path))
    (let lp ([tail path] [rparts '()])
      (if (string=? tail "")
          (if (null? rparts) "." (apply path-combine (reverse rparts)))
          (let ([first (path-first tail)])
            (cond
             [(string=? "" first) (lp "" (cons tail rparts))]
             [(string=? "." first) (lp (path-rest tail) rparts)]
             [(string=? ".." first)
              (lp (path-rest tail)
                (if abs?
                    (if (null? (cdr rparts))
                        rparts
                        (cdr rparts))
                    (if (or (null? rparts) (string=? (car rparts) ".."))
                        (cons ".." rparts)
                        (cdr rparts))))]
             [else (lp (path-rest tail) (cons first rparts))])))))
  )
