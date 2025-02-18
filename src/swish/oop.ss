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
(library (swish oop)
  (export
   base
   define-class
   this
   )
  (import
   (chezscheme)
   (swish dsm)
   )

  (define-record-type class (nongenerative)) ;; root class

  (meta define-record-type (ctcls $make-ctcls ctcls?)
    (nongenerative)
    (sealed #t)
    (fields
     (immutable name)                   ; symbol
     (immutable parent)                 ; #f | ctcls
     (immutable rtd)                    ; identifier
     (immutable rcd)                    ; #f | identifier
     (immutable make)                   ; #f | identifier
     (immutable fields)                 ; (ctfield ...)
     (immutable methods)                ; (ctmethod ...)
     (immutable virtuals)               ; (ctvirtual ...)
     (immutable overrides)              ; (ctvirtual ...)
     (immutable rtd^2)                  ; rtd of rtd
     (immutable names)                  ; name -> (field . ctfield) | (method ctmethod ...) |
                                        ; (isa?) | (make) | (this) | (base)
     ))

  (meta define-record-type ctfield
    (nongenerative)
    (sealed #t)
    (fields
     (immutable name)                   ; symbol
     (immutable offset)                 ; fixnum
     (immutable getter)                 ; identifier
     (immutable setter)                 ; #f | identifier
     ))

  (meta define-record-type ctmethod
    (nongenerative)
    (fields
     (immutable name)                   ; symbol
     (immutable arity)                  ; fixnum
     (immutable $id)                    ; identifier
     (immutable id)                     ; identifier
     ))

  (meta define-record-type ctvirtual
    (nongenerative)
    (sealed #t)
    (parent ctmethod)
    (fields
     (immutable offset)                 ; fixnum
     (immutable impl)                   ; identifier
     ))

  (meta define reserved-names
    '(base isa? make this))

  (meta define new-id
    (let ([top (car (generate-temporaries '(1)))])
      (lambda ls
        (let ([op (open-output-string)])
          (for-each (lambda (x) (display (syntax->datum x) op)) ls)
          (datum->syntax top (gensym (get-output-string op)))))))

  (meta define (rtd-field-count rtd)
    (length ((csv7:record-field-accessor #!base-rtd 'flds) rtd)))

  (meta define record-index->offset
    (let ()
      (define flds ((csv7:record-field-accessor #!base-rtd 'flds) #!base-rtd))
      (define (fld-offset x) (vector-ref x 4))
      (define first (fld-offset (car flds)))
      (define delta (- (fld-offset (cadr flds)) first))
      (lambda (i) (+ first (* i delta)))))

  (meta define (make-symbol-hashtable)
    (make-hashtable symbol-hash eq?))

  (meta define (make-class-rtd^2 name parent-rtd^2 virtuals)
    (if (null? virtuals)
        parent-rtd^2
        (make-record-type parent-rtd^2 (format "~a.rtd^2" name)
          (map (lambda (v)
                 (list 'immutable
                   (string->symbol (format "~a.~d" (ctmethod-name v) (ctmethod-arity v)))))
            virtuals))))

  (meta define (make-ctcls name parent rtd rcd make fields methods virtuals overrides)
    ($make-ctcls name parent rtd rcd make fields methods virtuals overrides
      (make-class-rtd^2 name (if parent (ctcls-rtd^2 parent) #!base-rtd) virtuals)
      (if parent
          (hashtable-copy (ctcls-names parent) #t)
          (let ([names (make-symbol-hashtable)])
            (symbol-hashtable-set! names 'isa? '(isa?))
            (symbol-hashtable-set! names 'make '(make))
            (symbol-hashtable-set! names '#{this eufjl7k5jwfpg31mjs5680p9d} '(this))
            (symbol-hashtable-set! names '#{base a36te8sh8d4jwl80hjktg92qdwn887ze} '(base))
            names))))

  (meta define root-ctcls
    (make-ctcls 'class #f #`(quote #,(type-descriptor class)) #f #f '() '() '() '()))

  (meta define (make-class-dispatcher ctcls)

    (define (expand-class-def ctcls def what args)
      (case (car def)
        [(field)
         (let ([arity (- (length args) 1)])
           (cond
            [(eqv? arity 0) (cons (ctfield-getter (cdr def)) args)]
            [(and (eqv? arity 1) (ctfield-setter (cdr def))) =>
             (lambda (setter) (cons setter args))]
            [else (bad-arity what ctcls "field")]))]
        [(method) (cons (ctmethod-id (find-arity (cdr def) (- (length args) 1) what ctcls)) args)]
        [(isa?)
         (syntax-case args ()
           [(e) #`(#3%record? e #,(ctcls-rtd ctcls))]
           [else (bad-arity what)])]
        [(make) (cons (ctcls-make ctcls) args)]
        [(this)
         (let ([what (car args)] [args (cdr args)])
           (expand-this-def ctcls
             (or (symbol-hashtable-ref names (syntax->datum what) #f)
                 (unknown-member what ctcls))
             what args))]
        [(base)
         (let ([what (car args)] [args (cdr args)] [parent (ctcls-parent ctcls)])
           (expand-base-def parent
             (or (symbol-hashtable-ref (ctcls-names parent) (syntax->datum what) #f)
                 (unknown-member what parent))
             what args))]))

    (define (expand-this-def ctcls def what args)
      (case (car def)
        [(field) (expand-field ctcls def what args)]
        [(method) (cons (ctmethod-$id (find-arity (cdr def) (- (length args) 1) what ctcls)) args)]
        [else (unknown-member what ctcls)]))

    (define (expand-base-def parent def what args)
      (case (car def)
        [(field) (expand-field parent def what args)]
        [(method)
         (let ([m (find-arity (cdr def) (- (length args) 1) what parent)])
           (if (ctvirtual? m)
               (cons (ctvirtual-impl m) args)
               (cons (ctmethod-$id m) args)))]
        [else (unknown-member what parent)]))

    (define (expand-field ctcls def what args)
      (with-syntax ([offset (ctfield-offset (cdr def))])
        (syntax-case args ()
          [(inst)
           #'(#%$object-ref 'scheme-object inst offset)]
          [(inst val)
           (ctfield-setter (cdr def))
           #'(#%$object-set! 'scheme-object inst offset val)]
          [else (bad-arity what ctcls "field")])))

    (define (find-arity ls arity what ctcls)
      (if (null? ls)
          (bad-arity what ctcls "instance method")
          (let ([m (car ls)])
            (if (= (ctmethod-arity m) arity)
                m
                (find-arity (cdr ls) arity what ctcls)))))

    (define bad-arity
      (case-lambda
       [(what c who)
        (syntax-error what
          (format "incorrect number of arguments to class ~a ~a" (ctcls-name c) who))]
       [(what)
        (syntax-error what "incorrect number of arguments to")]))

    (define (unknown-member what c)
      (syntax-error what (format "unknown class ~a member" (ctcls-name c))))

    (define names (ctcls-names ctcls))

    (define (insert! name updater)
      (symbol-hashtable-update! names name updater #f))

    (define (field f) (lambda (prev) (cons 'field f)))

    (define (method m)
      (lambda (prev)
        (cons 'method
          (cons m
            (if (and (pair? prev) (eq? (car prev) 'method))
                (let ([arity (ctmethod-arity m)])
                  (remp (lambda (m) (= (ctmethod-arity m) arity)) (cdr prev)))
                '())))))

    ;; fields
    (for-each (lambda (f) (insert! (ctfield-name f) (field f))) (ctcls-fields ctcls))

    ;; instance methods
    (let ([insert-instance-method! (lambda (m) (insert! (ctmethod-name m) (method m)))])
      (for-each insert-instance-method! (ctcls-methods ctcls))
      (for-each insert-instance-method! (ctcls-virtuals ctcls))
      (for-each insert-instance-method! (ctcls-overrides ctcls)))

    (lambda (x)
      (syntax-case x ()
        [(_ what arg ...)
         (identifier? #'what)
         (expand-class-def ctcls
           (or (symbol-hashtable-ref names (datum what) #f)
               (unknown-member #'what ctcls))
           #'what #'(arg ...))])))

  (define-syntax (base x)
    (syntax-error x "invalid context for"))

  (define-syntax define-class
    (let ()
      (define-syntactic-monad $p
        cte                             ; compile-time environment
        name                            ; identifier
        ht                              ; name -> field | (method arity ...)
        parent                          ; #f | ctcls
        fields                          ; #f | ((fname getter setter) ...)
        protocol                        ; #f | syntax
        methods                         ; ((mname arity $id id formals body) ...)
        virtuals                        ; ((mname arity impl formals body) ...)
        )

      (define-syntactic-monad $l
        name                            ; identifier
        parent                          ; ctcls
        fields                          ; ((offset fname getter setter) ...)
        protocol                        ; syntax
        methods                         ; ((mname arity $id id formals body) ...)
        virtuals                        ; ((offset id $id mname arity impl formals body) ...)
        overrides                       ; ((offset id $id mname arity impl formals body) ...)
        next-index                      ; fixnum
        )

      (define (doit cte name clauses)
        (let ([ht (make-symbol-hashtable)])
          (for-each (lambda (id) (symbol-hashtable-set! ht id 'reserved)) reserved-names)
          ($p parse ([parent #f]
                     [fields #f]
                     [protocol #f]
                     [methods '()]
                     [virtuals '()])
            clauses)))

      ($p define (parse clauses)
        (if (null? clauses)
            (let ([parent (or parent root-ctcls)]
                  [all-virtuals (reverse virtuals)])
              ($l layout-virtuals
                ([fields (if fields (layout-fields fields (total-field-count parent)) '())]
                 [protocol (or protocol #'#f)]
                 [methods (reverse methods)]
                 [virtuals '()]
                 [overrides '()]
                 [next-index (rtd-field-count (ctcls-rtd^2 parent))])
                all-virtuals
                (ctcls-names parent)))
            (let ([clause (car clauses)])
              (syntax-case clause ()
                [(kw:parent parent-name)
                 (and (eq? (datum kw:parent) 'parent) (identifier? #'parent-name))
                 (cond
                  [parent (syntax-error clause "duplicate definition")]
                  [(cte #'parent-name #'class) => (lambda (parent) ($p parse () (cdr clauses)))]
                  [else (syntax-error #'parent-name "unrecognized parent class")])]
                [(kw:fields fspec ...)
                 (eq? (datum kw:fields) 'fields)
                 (cond
                  [fields (syntax-error clause "duplicate definition")]
                  [else
                   ($p parse
                     ([fields (map (lambda (fspec) (parse-field fspec name ht)) #'(fspec ...))])
                     (cdr clauses))])]
                [(kw:protocol protocol-e)
                 (eq? (datum kw:protocol) 'protocol)
                 (cond
                  [protocol (syntax-error clause "duplicate definition")]
                  [else ($p parse ([protocol #'protocol-e]) (cdr clauses))])]
                [(kw:method (mname formal ...) body ...)
                 (and (eq? (datum kw:method) 'method) (for-all identifier? #'(mname formal ...)))
                 (let ([arity (length #'(formal ...))])
                   (add-name! ht #'mname 'method arity clause)
                   ($p parse
                     ([methods
                       (cons (list #'mname arity (new-id "$" name "." #'mname "." arity)
                               (new-id name "." #'mname "." arity)
                               #'(formal ...) (make-body #'(body ...))) methods)])
                     (cdr clauses)))]
                [(kw:virtual (mname formal ...) body ...)
                 (and (eq? (datum kw:virtual) 'virtual) (for-all identifier? #'(mname formal ...)))
                 (let ([arity (length #'(formal ...))])
                   (add-name! ht #'mname 'method arity clause)
                   ($p parse
                     ([virtuals
                       (cons (list #'mname arity (new-id "$" name "." #'mname "." arity ".impl")
                               #'(formal ...) (make-body #'(body ...)))
                         virtuals)])
                     (cdr clauses)))]))))

      (define (parse-field fspec name ht)
        (syntax-case fspec ()
          [(ftype fname)
           (and (memq (datum ftype) '(immutable mutable)) (identifier? #'fname))
           (begin
             (add-name! ht #'fname 'field #f fspec)
             (list #'fname (new-id name "." #'fname)
               (and (eq? (datum ftype) 'mutable) (new-id name "." #'fname ".set!"))))]))

      (define (add-name! ht id type arity clause)
        (let* ([name (syntax->datum id)]
               [cell (symbol-hashtable-cell ht name #f)]
               [val (cdr cell)])
          (cond
           [(not val) (set-cdr! cell (if arity (list type arity) type))]
           [(eq? val 'reserved) (syntax-error id "reserved name")]
           [(and arity (pair? val) (eq? (car val) type) (not (memv arity (cdr val))))
            (set-cdr! val (cons arity (cdr val)))]
           [else (syntax-error clause "duplicate definition")])))

      (define (make-body ls)
        (cond
         [(null? ls) #'(void)]
         [(null? (cdr ls)) (car ls)]
         [else (cons #'begin ls)]))

      (define (layout-fields ls index)
        (if (null? ls)
            '()
            (cons (list* (record-index->offset index) (car ls))
              (layout-fields (cdr ls) (+ index 1)))))

      ($l define (layout-virtuals ls names)
        (if (null? ls)
            ($l layout ([virtuals (reverse virtuals)] [overrides (reverse overrides)]))
            (let ([x (car ls)])
              (let ([mname (car x)] [arity (cadr x)])
                (cond
                 [(lookup-virtual names mname arity) =>
                  (lambda (v)
                    ($l layout-virtuals
                      ([overrides
                        (cons (list* (ctvirtual-offset v) (ctmethod-id v) (ctmethod-$id v) x)
                          overrides)])
                      (cdr ls) names))]
                 [else
                  ($l layout-virtuals
                    ([virtuals
                      (cons (list* (record-index->offset next-index)
                              (new-id name "." mname "." arity)
                              (new-id "$" name "." mname "." arity)
                              x) virtuals)]
                     [next-index (+ next-index 1)])
                    (cdr ls) names)])))))

      (define (lookup-virtual names mname arity)
        (let ([def (symbol-hashtable-ref names (syntax->datum mname) #f)])
          (and def
               (eq? (car def) 'method)
               (find (lambda (m) (and (ctvirtual? m) (= (ctmethod-arity m) arity))) (cdr def)))))

      ($l define (layout)
        (list
         parent
         (ctcls-rcd parent)
         (new-id name ".rtd")
         (new-id name ".rcd")
         protocol
         (new-id name ".make")
         ;; ((field-offset field-name name.field field-setter) ...)
         fields
         ;; ((method-name method-arity $name.method.arity name.method.arity
         ;;    method-formals method-body) ...)
         methods
         ;; ((virtual-offset name.virtual.arity $name.virtual.arity virtual-name virtual-arity
         ;;    $name.virtual.arity.impl virtual-formals virtual-body) ...)
         virtuals
         ;; ((override-offset override-id override-$id override-name override-arity
         ;;    $name.override.arity.impl override-formals override-body) ...)
         overrides
         ;; ((name.mutable-field.set! mutable-field-name mutable-field-offset) ...)
         (let lp ([fields fields])
           (if (null? fields)
               '()
               (apply
                (lambda (offset fname getter setter)
                  (if setter
                      (cons (list setter fname offset) (lp (cdr fields)))
                      (lp (cdr fields))))
                (car fields))))))

      (define (total-field-count ctcls)
        (let ([n (length (ctcls-fields ctcls))])
          (cond
           [(ctcls-parent ctcls) => (lambda (parent) (+ (total-field-count parent) n))]
           [else n])))

      (lambda (x)
        (syntax-case x ()
          [(_ name clause ...)
           (identifier? #'name)
           (lambda (cte)
             (with-syntax
              ([(parent-ctcls
                 parent-rcd
                 name.rtd
                 name.rcd
                 protocol
                 name.make
                 ((field-offset field-name name.field field-setter) ...)
                 ((method-name method-arity $name.method.arity name.method.arity
                    method-formals method-body) ...)
                 ((virtual-offset name.virtual.arity $name.virtual.arity virtual-name virtual-arity
                    $name.virtual.arity.impl virtual-formals virtual-body) ...)
                 ((override-offset override-id override-$id override-name override-arity
                    $name.override.arity.impl override-formals override-body) ...)
                 ((name.mutable-field.set! mutable-field-name mutable-field-offset) ...)
                 )
                (doit cte #'name #'(clause ...))])
              #'(module ((name
                          $name.virtual.arity.impl ...
                          $name.override.arity.impl ...
                          name.rtd
                          name.rcd
                          name.make
                          name.field ...
                          name.mutable-field.set! ...
                          $name.method.arity ...
                          name.method.arity ...
                          $name.virtual.arity ...
                          name.virtual.arity ...
                          ))
                  (meta define ctcls
                    (make-ctcls 'name 'parent-ctcls #'name.rtd #'name.rcd #'name.make
                      (list (make-ctfield 'field-name field-offset #'name.field
                              (and 'field-setter #'field-setter)) ...)
                      (list (make-ctmethod 'method-name method-arity #'$name.method.arity
                              #'name.method.arity) ...)
                      (list (make-ctvirtual 'virtual-name virtual-arity #'$name.virtual.arity
                              #'name.virtual.arity virtual-offset #'$name.virtual.arity.impl) ...)
                      (list (make-ctvirtual 'override-name override-arity #'override-$id
                              #'override-id override-offset #'$name.override.arity.impl) ...)))
                  (define-syntax name (make-class-dispatcher ctcls))
                  (define-property name class ctcls)
                  (define ($name.virtual.arity.impl inst . virtual-formals)
                    (open-instance name inst virtual-formals virtual-body)) ...
                  (define ($name.override.arity.impl inst . override-formals)
                    (open-instance name inst override-formals override-body)) ...
                  (define name.rtd (make-class-rtd name))
                  (define name.rcd
                    (make-record-constructor-descriptor name.rtd parent-rcd protocol))
                  (define name.make (record-constructor name.rcd))
                  (define (name.field inst)
                    (record-check 'field-name inst name.rtd)
                    (#%$object-ref 'scheme-object inst field-offset)) ...
                  (define (name.mutable-field.set! inst x)
                    (record-check 'mutable-field-name inst name.rtd)
                    (#%$object-set! 'scheme-object inst mutable-field-offset x)) ...
                  (define ($name.method.arity inst . method-formals)
                    (open-instance name inst method-formals method-body)) ...
                  (define (name.method.arity inst . method-formals)
                    (record-check 'method-name inst name.rtd)
                    ($name.method.arity inst . method-formals)) ...
                  (define ($name.virtual.arity inst . virtual-formals)
                    ((#%$object-ref 'scheme-object (#3%record-rtd inst) virtual-offset)
                     inst . virtual-formals)) ...
                  (define (name.virtual.arity inst . virtual-formals)
                    (record-check 'virtual-name inst name.rtd)
                    ($name.virtual.arity inst . virtual-formals)) ...)))]))))

  (define-syntax (this x)
    (syntax-error x "invalid context for"))

  (define-syntax (make-class-rtd x)
    (syntax-case x ()
      [(_ name)
       (lambda (cte)
         (define (make-field-spec f)
           (list (if (ctfield-setter f) 'mutable 'immutable) (ctfield-name f)))
         (define (make-virtual-impls ctcls)
           (define ht (make-hashtable values fx=)) ; offset -> virtual-impl
           (define (add! v) (hashtable-set! ht (ctvirtual-offset v) (ctvirtual-impl v)))
           (let gather ([ctcls ctcls])
             (cond [(ctcls-parent ctcls) => gather])
             (for-each add! (ctcls-virtuals ctcls))
             (for-each add! (ctcls-overrides ctcls)))
           (let emit ([i (rtd-field-count (ctcls-rtd^2 ctcls))]
                      [lo (rtd-field-count #!base-rtd)]
                      [impls '()])
             (if (> i lo)
                 (let ([i (- i 1)])
                   (emit i lo
                     (cons (or (hashtable-ref ht (record-index->offset i) #f)
                               (errorf 'make-class-rtd "missing virtual for index ~d" i))
                       impls)))
                 impls)))
         (define ctcls (cte #'name #'class))
         (with-syntax ([rtd^2 (ctcls-rtd^2 ctcls)]
                       [parent-rtd (ctcls-rtd (ctcls-parent ctcls))]
                       [field-specs (datum->syntax #'_
                                      (list->vector (map make-field-spec (ctcls-fields ctcls))))]
                       [virtual-impls (make-virtual-impls ctcls)])
           #'(#%$make-record-type-descriptor 'rtd^2 'name parent-rtd #f #f #f 'field-specs #f
               . virtual-impls)))]))

  (define-syntax (open-instance x)
    (syntax-case x ()
      [(_ cname inst formals body)
       (lambda (cte)
         (define-syntactic-monad $o
           ctcls                ; ctcls
           names                ; #(name ...)
           defs                 ; #({(field . ctfield) | (method ctmethod ...) | (_)} ...)
           immutable-fields     ; ((fname . offset) ...)
           mutable-fields       ; ((fname . offset) ...)
           methods              ; ((mname ($id arg ...) ...) ...)
           )
         ($o define (gather i)
           (if (= i (vector-length names))
               ($o expand)
               (let ([name (vector-ref names i)] [def (vector-ref defs i)])
                 (let ([type (car def)] [val (cdr def)] [name-id (datum->syntax #'cname name)])
                   (case type
                     [(field)
                      (if (ctfield-setter val)
                          ($o gather ([mutable-fields (cons (cons name-id (ctfield-offset val))
                                                        mutable-fields)]) (+ i 1))
                          ($o gather ([immutable-fields (cons (cons name-id (ctfield-offset val))
                                                          immutable-fields)]) (+ i 1)))]
                     [(method)
                      ($o gather
                        ([methods (cons (cons name-id
                                          (map (lambda (m)
                                                 (cons (ctmethod-$id m)
                                                   (generate-temporaries (iota (ctmethod-arity m)))))
                                            (cdr def))) methods)])
                        (+ i 1))]
                     [else ($o gather () (+ i 1))])))))
         ($o define (expand)
           (with-syntax ([parent? (not (eq? (ctcls-parent ctcls) root-ctcls))]
                         [((immutable-field . immutable-field-offset) ...) immutable-fields]
                         [((mutable-field . mutable-field-offset) ...) mutable-fields]
                         [((method (method-$id . method-args) ...) ...) methods])
             #'((fluid-let-syntax
                 ([this
                   (lambda (x)
                     (syntax-case x ()
                       [(__ what . args)
                        (identifier? #'what)
                        #'(cname #{this eufjl7k5jwfpg31mjs5680p9d} what inst . args)]
                       [__ (identifier? #'__) #'inst]))]
                  [base
                   (lambda (x)
                     (if parent?
                         (syntax-case x ()
                           [(__ what . args)
                            (identifier? #'what)
                            #'(cname #{base a36te8sh8d4jwl80hjktg92qdwn887ze} what inst . args)])
                         (syntax-error x "no parent for")))])
                 (let-syntax
                     ([immutable-field
                       (identifier-syntax (#%$object-ref 'scheme-object inst immutable-field-offset))]
                      ...
                      [mutable-field
                       (identifier-syntax
                        [id (#%$object-ref 'scheme-object inst mutable-field-offset)]
                        [(set! id val)
                         (#%$object-set! 'scheme-object inst mutable-field-offset val)])]
                      ...
                      [method
                       (lambda (x)
                         (syntax-case x ()
                           [(__ . method-args) #'(method-$id inst . method-args)]
                           ...))]
                      ...)
                   (lambda formals body))) . formals)))
         (let ([ctcls (cte #'cname #'class)])
           (let-values ([(names defs) (hashtable-entries (ctcls-names ctcls))])
             ($o gather ([immutable-fields '()] [mutable-fields '()] [methods '()]) 0))))]))

  (define-syntax record-check
    (syntax-rules ()
      [(_ who inst rtd)
       (and (identifier? #'inst) (identifier? #'rtd))
       (unless (#3%record? inst rtd)
         (#%$record-oops who inst rtd))])))
