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
(library (swish erlang)
  (export
   arg-check
   bad-arg
   catch
   define-match-extension
   define-tuple
   dump-stack
   limit-stack
   limit-stack?
   make-fault
   make-fault/no-cc
   make-process-parameter
   match
   match-define
   match-let*
   on-exit
   profile-me
   reset-process-parameters!
   throw
   try
   walk-stack
   walk-stack-max-depth
   )
  (import
   (chezscheme)
   (swish internal)
   (swish meta)
   )
  ;; Procedures starting with @ must be called with interrupts disabled.

  (define-syntax (arg-check x)
    (syntax-case x ()
      [(k $who [$arg pred ...] ...)
       #'(let ([who $who])
           (let ([arg $arg])
             (unless (and (pred arg) ...)
               ;; count on arg-check indicates test coverage of bad-arg case
               (profile-me-as k)
               (bad-arg who arg)))
           ...
           (void))]))

  (define-syntax on-exit
    (syntax-rules ()
      [(_ finally b1 b2 ...)
       (dynamic-wind
         void
         (lambda () b1 b2 ...)
         (lambda () finally))]))

  ($import-internal
   make-fault
   make-fault/no-cc
   &fault-condition fault-condition-reason fault-condition? make-fault-condition)

  (define (unwrap-fault-condition r)
    (if (fault-condition? r)
        (fault-condition-reason r)
        r))

  (define (->fault-condition reason)
    (if (fault-condition? reason)
        reason
        (make-fault-condition #f reason '())))

  (define (->EXIT reason)
    `#(EXIT ,(unwrap-fault-condition reason)))

  (define-syntax try
    (syntax-rules ()
      [(_ e1 e2 ...)
       ($trap (lambda () e1 e2 ...) ->fault-condition)]))

  ;; This binding serves a dual purpose: it provides backwards
  ;; compatibility for older code and it provides a binding for
  ;; the define-match-extension used in newer code.
  (define-syntax catch
    (syntax-rules ()
      [(_ e1 e2 ...)
       ($trap (lambda () e1 e2 ...) ->EXIT)]))

  (define ($trap thunk ->reason)
    (call/1cc
     (lambda (return)
       (with-exception-handler
        (lambda (r)
          (return (->reason r)))
        thunk))))

  ($import-internal throw)

  (define (bad-arg who arg)
    (throw `#(bad-arg ,who ,arg)))

  (define dump-stack
    (let ()
      (define (source-path src)
        (and (source-object? src)
             (let ([sfd (source-object-sfd src)])
               (call-with-values
                 (lambda () (locate-source sfd (source-object-bfp src) #t))
                 (case-lambda
                  [()
                   (format " at offset ~a of ~a" (source-object-bfp src)
                     (source-file-descriptor-path sfd))]
                  [(path line char)
                   (format " at line ~a, char ~a of ~a" line char path)])))))
      (case-lambda
       [() (dump-stack (current-output-port))]
       [(op) (call/cc (lambda (k) (dump-stack k op 'default)))]
       [(k op max-depth)
        (define (dump-frame name source proc-source vars)
          (display name op)
          (cond
           [(source-path source) => (lambda (where) (display where op))]
           [(source-path proc-source) =>
            (lambda (where) (fprintf op " in procedure~a" where))])
          (newline op)
          (for-each
           (lambda (var)
             (fprintf op "  ~s: ~s\n" (car var) (cdr var)))
           (or vars '())))
        (define (next-frame frame base depth next) (next base))
        (define (truncated base depth)
          (fprintf op "Stack dump truncated due to max-depth = ~s.\n" depth))
        (unless (output-port? op) (bad-arg 'dump-stack op))
        (parameterize ([print-level 3] [print-length 6] [print-gensym #f] [print-extended-identifiers #t])
          (walk-stack k (void) dump-frame next-frame
            'dump-stack max-depth truncated))])))

  (define ($limit-stack thunk source)
    ;; thwart cp0 and ensure source is live on the stack
    ((call-with-values thunk $limit-stack-receiver) source))

  (define $limit-stack-receiver
    (case-lambda
     [(x) (lambda (source) x)]
     [xs (lambda (source) (apply values xs))]))

  (define-syntax (limit-stack x)
    (syntax-case x ()
      [(ls e0 e1 ...)
       #`($limit-stack (lambda () e0 e1 ...)
           #,(find-source #'ls))]))

  (define (limit-stack? k)
    (and (#3%$continuation? k)
         (eq? (#3%$continuation-return-code k)
           (#3%$closure-code $limit-stack))))

  (define (do-frame cont handle-frame)
    (let ([description (format "~s" (cont 'value))]
          [source (cont 'source-object)]
          [proc-source ((cont 'code) 'source-object)]
          [vars
           (do ([i (fx1- (cont 'length)) (fx1- i)]
                [vars '()
                  (let ([var (cont 'ref i)])
                    (cons (cons (or (var 'name) i) ((var 'ref) 'value))
                      vars))])
               ((fx< i 0) vars))])
      (handle-frame description source proc-source vars)))

  (define walk-stack-max-depth
    (make-parameter 10
      (lambda (v)
        (arg-check 'walk-stack-max-depth
          [v fixnum? fxnonnegative?])
        v)))

  (define walk-stack
    (case-lambda
     [(k base handle-frame combine)
      (walk-stack k base handle-frame combine 'walk-stack 'default
        (lambda (base depth) base))]
     [(k base handle-frame combine who max-depth truncated)
      (define in (if (symbol? who) who 'walk-stack))
      (arg-check in
        [handle-frame procedure?]
        [who symbol?]
        [combine procedure?]
        [truncated procedure?])
      (let ([obey-limit? (eq? max-depth 'default)]
            [max-depth
             (match max-depth
               [default (walk-stack-max-depth)]
               [,n (guard (and (fixnum? n) (positive? n))) n]
               [#f (most-positive-fixnum)]
               [,_ (bad-arg in max-depth)])])
        (let loop ([cont (inspect/object k)] [depth 0] [base base])
          (cond
           [(not (eq? (cont 'type) 'continuation)) base]
           [(fx= depth max-depth) (truncated base depth)]
           [else
            ;; force evaluation order in case do-frame has side effects
            (let ([frame (do-frame cont handle-frame)])
              (combine frame base depth
                (if (and obey-limit? (limit-stack? (cont 'value)))
                    (lambda (base) base)
                    (lambda (base)
                      (loop (cont 'link) (+ depth 1) base)))))])))]))

  ;; to get better names for match continuations than native or
  (define-syntax match-or
    (syntax-rules ()
      [(_) #f]
      [(_ e) e]
      [(_ e0 e1 ...)
       (let ([matched-pattern e0]) ;; could we get actual source info for these?
         (if matched-pattern matched-pattern (match-or e1 ...)))]))

  (define-syntax (match x)
    (syntax-case x ()
      [(_ exp (pattern (guard g) b1 b2 ...))
       (eq? (datum guard) 'guard)
       #`(match-let* ([pattern (guard g) exp]) b1 b2 ...)]
      [(_ exp (pattern b1 b2 ...))
       #`(match-let* ([pattern exp]) b1 b2 ...)]
      [(_ exp (pattern b1 b2 ...) ...)
       #`(let ([v exp])
           ((match-or (match-pattern v pattern b1 b2 ...) ...
              (bad-match v #,(find-source x)))))]))

  (define-syntax match-pattern
    (syntax-rules ()
      [(_ e pat (guard g) b1 b2 ...)
       (eq? (datum guard) 'guard)
       (match-one e pat fail-false (and g (lambda () b1 b2 ...)))]
      [(_ e pat b1 b2 ...)
       (match-one e pat fail-false (lambda () b1 b2 ...))]))

  (define-syntax (match-let* x)
    (syntax-case x ()
      [(_ () b1 b2 ...)
       #'(let () b1 b2 ...)]
      [(_ ([pattern exp] . rest) b1 b2 ...)
       #`(let ([v exp])
           (let-syntax ([fail
                         (syntax-rules ()
                           [(__) (bad-match v #,(find-source #'pattern))])])
             (match-one v pattern fail (match-let* rest b1 b2 ...))))]
      [(_ ([pattern (guard g) exp] . rest) b1 b2 ...)
       (eq? (datum guard) 'guard)
       #`(let ([v exp])
           (let-syntax ([fail
                         (syntax-rules ()
                           [(__) (bad-match v #,(find-source #'pattern))])])
             (match-one v pattern fail
               (if g
                   (match-let* rest b1 b2 ...)
                   (bad-match v #,(find-source #'g))))))]))

  (define-syntax fail-false
    (syntax-rules ()
      [(_) #f]))

  (define (bad-match v src)
    (throw `#(bad-match ,v ,src)))

  (define extension)
  (define extensions)

  (define-syntax (define-match-extension x)
    (syntax-case x ()
      [(_ type handle-object-expr)
       #'(define-match-extension type handle-object-expr
           (lambda x
             (errorf 'define-match-extension
               "no handle-field procedure provided for ~s" 'type)))]
      [(_ type handle-object-expr handle-field-expr)
       #'(begin
           (define-syntax handle-object
             (make-compile-time-value handle-object-expr))
           (define-syntax handle-field
             (make-compile-time-value handle-field-expr))
           (define-property type extensions
             #'(extension handle-object handle-field)))]))

  (meta define (get-rtd lookup id)
    (let ([hit (lookup id)])
      (and (list? hit)
           (apply
            (case-lambda
             [(ignore1 rtd . ignore2)
              (and (record-type-descriptor? rtd) rtd)]
             [other #f])
            hit))))

  (meta define (get-extended lookup type)
    (define (reject-options options context)
      (syntax-case options ()
        [() (void)]
        [_ (pretty-syntax-violation "invalid match pattern" context)]))
    (cond
     [(not (identifier? type)) 'bad-pattern]
     [(lookup type #'extensions) =>
      (lambda (x)
        (syntax-case x (extension)
          [(extension handle-object handle-field)
           (values
            (lookup #'handle-object)
            (lookup #'handle-field))]))]
     [(lookup type #'fields) =>
      ;; tuple
      (lambda (fields)
        (values
         (lambda (v pattern)
           ;; handle-object
           (syntax-case pattern (quasiquote)
             [`(type spec ...)
              #`((guard (type is? #,v))
                 ;; could convert to sub-match, but this is easier
                 (handle-fields (#,v type) spec ...))]))
         ;; handle-field
         (lambda (input fld var options context)
           (reject-options options context)
           (and (memq (syntax->datum fld) fields)
                (syntax-case input ()
                  [(v type)
                   #`((bind #,var (type no-check #,fld v)))])))))]
     [(get-rtd lookup type) =>
      ;; native record
      (lambda (rtd)
        (values
         ;; handle-object
         (lambda (v pattern)
           (syntax-case pattern (quasiquote)
             [`(type spec ...)
              #`((guard ((#3%record-predicate '#,rtd) #,v))
                 (handle-fields #,v spec ...))]))
         ;; handle-field
         (lambda (v fld var options context)
           (reject-options options context)
           (and (guard (c [else #f])
                  (csv7:record-field-accessible? rtd (syntax->datum fld)))
                #`((bind #,var
                     ((#3%csv7:record-field-accessor '#,rtd '#,fld) #,v)))))))]
     [else #f]))

  ;; fail is currently propagated unchanged
  (meta define (match-help lookup x nested?)
    (define (bad-pattern x)
      (syntax-case x ()
        [(e pat . rest)
         (pretty-syntax-violation "invalid match pattern" #'pat)]))
    (define (bad-syntax context msg detail)
      (syntax-case context ()
        [(e pat . rest)
         (pretty-syntax-violation msg #'pat detail)]))
    (define reject-duplicate
      (let ([bound-vars '()])
        (define (duplicate? id)
          (lambda (b) (bound-identifier=? b id)))
        (lambda (id)
          (if (ormap (duplicate? id) bound-vars)
              (syntax-error id "duplicate pattern variable")
              (set! bound-vars (cons id bound-vars))))))
    (define-syntax bind-help
      (syntax-rules ()
        [(_ v e expr)
         (if nested?
             #`(let ([v e]) #,expr)
             #`(begin (define v e) #,expr))]))
    (define-syntax bind-var
      (syntax-rules ()
        [(_ v e expr)
         (let ()
           (reject-duplicate #'v)
           (bind-help v e expr))]))
    (define-syntax fresh-var
      (syntax-rules ()
        [(_ v e expr)
         (with-temporaries (v)
           (bind-help v e expr))]))
    (define-syntax test
      (syntax-rules ()
        [(_ pred body fail)
         (if nested?
             #`(if pred body (fail))
             (with-temporaries (tmp)
               #`(begin (define tmp (unless pred (fail))) body)))]))
    (define (generate context object? handle-field fail body ir)
      (let f ([ir ir])
        (syntax-case ir ()
          [#f (bad-pattern x)]
          [() body]
          [((bind v e) . more)
           (eq? (datum bind) 'bind)
           (bind-var v e (f #'more))]
          [((guard g) . more)
           (eq? (datum guard) 'guard)
           (test g #,(f #'more) #,fail)]
          [((sub-match v pattern))
           (and object? (eq? (datum sub-match) 'sub-match))
           ;; sub-match pattern does not allow guard
           (convert #`(v pattern #,fail #,body))]
          [((handle-fields v field-spec ...))
           (and object? (eq? (datum handle-fields) 'handle-fields))
           (let ()
             (define (do-field field dest-var options body)
               ;; We don't check whether handler emits a bind for dest-var.
               ;; If the code is in a library, the compiler will complain for us.
               (generate context #f #f fail body
                 (or (handle-field #'v field dest-var options context)
                     (bad-syntax x "unknown field" field))))
             (let match-extended-help ([specs #'(field-spec ...)])
               (syntax-case specs (unquote unquote-splicing)
                 [() body]
                 [((unquote field) . rest)
                  (if (identifier? #'field)
                      (do-field #'field #'field '()
                        (match-extended-help #'rest))
                      (bad-pattern x))]
                 [((unquote-splicing var) . rest)
                  (if (identifier? #'var)
                      (with-temporaries (tmp)
                        (do-field #'var #'tmp '()
                          (test (equal? tmp var)
                            #,(match-extended-help #'rest)
                            #,fail)))
                      (bad-pattern x))]
                 [([field pattern option ...] . rest)
                  (with-temporaries (tmp)
                    (do-field #'field #'tmp #'(option ...)
                      (convert #`(tmp pattern #,fail #,(match-extended-help #'rest)))))]
                 [other (bad-pattern x)])))]
          [oops
           (pretty-syntax-violation
            (if object?
                "invalid handle-object output"
                "invalid handle-field output")
            context #'oops 'define-match-extension)])))
    (define (convert x)
      (syntax-case x (unquote unquote-splicing quasiquote)
        [(e (unquote (v <= pattern)) fail body)
         (and (identifier? #'v) (eq? (datum <=) '<=))
         (bind-var v e (convert #'(v pattern fail body)))]
        [(e (unquote v) fail body)
         (cond
          [(eq? (datum v) '_) #'body]
          [(identifier? #'v) (bind-var v e #'body)]
          [else (bad-pattern x)])]
        [(e (unquote-splicing var) fail body)
         (if (identifier? #'var)
             (test (equal? e var) body fail)
             (bad-pattern x))]
        [(e (quasiquote (type spec ...)) fail body)
         (call-with-values
           (lambda () (get-extended lookup #'type))
           (case-lambda
            [(fail)
             (cond
              [(eq? fail 'bad-pattern) (bad-pattern x)]
              [else (bad-syntax x "unknown type" #'type)])]
            [(handle-object handle-field)
             ;; extract pattern, don't rebuild or we'll get wrong source info
             (let ([pattern (syntax-case x () [(_ pat . _) #'pat])])
               (fresh-var v e
                 (generate pattern
                   #t handle-field #'fail #'body
                   (handle-object #'v pattern))))]))]
        [(e lit fail body)
         (let ([x (datum lit)])
           (or (symbol? x) (number? x) (boolean? x) (char? x)))
         (fresh-var v e (test (eqv? v 'lit) body fail))]
        [(e s fail body)
         (string? (datum s))
         (fresh-var v e (test (and (string? v) (#3%string=? v s)) body fail))]
        [(e eof fail body)
         (eof-object? (datum eof))
         (fresh-var v e (test (eof-object? v) body fail))]
        [(e bv fail body)
         (bytevector? (datum bv))
         (fresh-var v e (test (and (bytevector? v) (#3%bytevector=? v bv)) body fail))]
        [(e () fail body)
         (fresh-var v e (test (null? v) body fail))]
        [(e (first . rest) fail body)
         (fresh-var v e
           (test (pair? v)
             #,(convert
                #`((#3%car v) first fail
                   #,(convert #'((#3%cdr v) rest fail body))))
             fail))]
        [(e #(element ...) fail body)
         (let ([len (length (datum (element ...)))])
           (fresh-var v e
             (test (and (vector? v)
                        (#3%fx= (#3%vector-length v) #,len))
               #,(let lp ([i 0] [elt* #'(element ...)])
                   (if (= i len)
                       #'body
                       (convert
                        #`((#3%vector-ref v #,i)
                           #,(car elt*) fail #,(lp (+ i 1) (cdr elt*))))))
               fail)))]
        [_ (bad-pattern x)]))
    (convert x))

  (define-syntax (match-one x)
    (syntax-case x ()
      [(_ e pattern fail body)
       (lambda (lookup)
         (match-help lookup #'(e pattern fail body) #t))]))

  (define-syntax (match-define x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ pattern e)
         #`(begin
             (define v e)
             (define (fail) (pariah (bad-match v #,(find-source #'pattern))))
             #,(match-help lookup
                 #'(v pattern fail (begin)) #f))])))

  (meta define (generate-name prefix fn)
    (if (not prefix) fn (compound-id fn prefix fn)))

  (meta define (get-binding-names bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (cons #'fn (get-binding-names #'rest))]
      [() '()]))

  (meta define (remove-binding f bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (if (syntax-datum-eq? #'fn f)
           #'rest
           #`((fn fv) #,@(remove-binding f #'rest)))]))

  (meta define (find-index fn fields index)
    (let ([f (scar fields)])
      (if (syntax-datum-eq? f fn)
          index
          (find-index fn (scdr fields) (+ index 1)))))

  (meta define (find-binding f bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (if (syntax-datum-eq? #'fn f)
           #'fv
           (find-binding f #'rest))]
      [() #f]))

  (define-syntax (define-tuple x)
    (syntax-case x ()
      [(_ name field ...)
       (and (identifier? #'name)
            (valid-fields? x #'(field ...) #f '(make copy copy* is?)))
       #'(begin
           (define-syntax (name x)
             (define (handle-open x expr prefix field-names)
               (define (make-accessor fn)
                 (let ([new-name (generate-name prefix fn)])
                   #`(define-syntax #,new-name (identifier-syntax (name no-check #,fn tmp)))))
               (if (not (valid-fields? x field-names '(field ...) '()))
                   (syntax-case x ())
                   #`(begin
                       (define tmp
                         (let ([val #,expr])
                           (unless (name is? val)
                             (throw `#(bad-tuple name ,val ,#,(find-source x))))
                           val))
                       #,@(map make-accessor (syntax->list field-names)))))
             (define (handle-copy x e bindings mode)
               #`(let ([src #,e])
                   #,(case mode
                       [copy
                        #`(unless (name is? src)
                            (throw `#(bad-tuple name ,src ,#,(find-source x))))]
                       [copy*
                        (handle-open x #'src #f (get-binding-names bindings))])
                   (vector 'name #,@(copy-tuple #'(field ...) 1 bindings))))
             (define (valid-bindings? bindings)
               (valid-fields? x (get-binding-names bindings) '(field ...) '()))
             (define (make-tuple fields bindings)
               (if (snull? fields)
                   '()
                   (let* ([f (scar fields)]
                          [v (find-binding f bindings)])
                     (unless v
                       (syntax-error x
                         (format "missing field ~a in" (syntax->datum f))))
                     (cons v
                       (make-tuple (scdr fields) (remove-binding f bindings))))))
             (define (copy-tuple fields index bindings)
               (if (snull? fields)
                   '()
                   (let* ([f (scar fields)]
                          [v (find-binding f bindings)])
                     (if v
                         (cons v (copy-tuple (scdr fields) (+ index 1)
                                   (remove-binding f bindings)))
                         (cons #`(#3%vector-ref src #,(datum->syntax f index))
                           (copy-tuple (scdr fields) (+ index 1) bindings))))))

             (syntax-case x ()
               [(_name make . bindings)
                (and (eq? (datum make) 'make)
                     (valid-bindings? #'bindings))
                #`(vector 'name #,@(make-tuple #'(field ...) #'bindings))]
               [(name copy e . bindings)
                (and (eq? (datum copy) 'copy)
                     (valid-bindings? #'bindings))
                (handle-copy x #'e #'bindings 'copy)]
               [(name copy* e . bindings)
                (and (eq? (datum copy*) 'copy*)
                     (valid-bindings? #'bindings))
                (handle-copy x #'e #'bindings 'copy*)]
               [(name open expr prefix field-names)
                (and (eq? (datum open) 'open) (identifier? #'prefix))
                (handle-open x #'expr #'prefix #'field-names)]
               [(name open expr field-names)
                (eq? (datum open) 'open)
                (handle-open x #'expr #f #'field-names)]
               [(name is? . args)
                (eq? (datum is?) 'is?)
                (let ([is?
                       #'(lambda (x)
                           (and (vector? x)
                                (#3%fx= (#3%vector-length x) (length '(name field ...)))
                                (eq? (#3%vector-ref x 0) 'name)))])
                  (syntax-case #'args ()
                    [() is?]
                    [(e) #`(#,is? e)]
                    [else (syntax-case x ())]))]
               [(name fn e)
                (syntax-datum-eq? #'fn #'field)
                (with-syntax ([getter (replace-source x #'(name fn))])
                  #`(getter e))]
               ...
               [(name fn)
                (syntax-datum-eq? #'fn #'field)
                #`(lambda (x)
                    (unless (name is? x)
                      (throw `#(bad-tuple name ,x ,#,(find-source x))))
                    (#3%vector-ref x #,(find-index #'fn #'(field ...) 1)))]
               ...
               [(name no-check fn e)
                (and (eq? (datum no-check) 'no-check)
                     (syntax-datum-eq? #'fn #'field))
                #`(#3%vector-ref e #,(find-index #'fn #'(field ...) 1))]
               ...
               [(name fn)
                (identifier? #'fn)
                (syntax-violation #f "unknown field" x #'fn)]
               [(name fn expr)
                (identifier? #'fn)
                (syntax-violation #f "unknown field" x #'fn)]
               ))
           (define-property name fields '(field ...)))]))

  (define (legacy-EXIT? x)
    (and (vector? x)
         (#3%fx= (#3%vector-length x) 2)
         (eq? (#3%vector-ref x 0) 'EXIT)))

  (define-syntax direct-&fault-condition-ref
    (syntax-rules ()
      [(_ field x)
       ((#3%csv7:record-field-accessor (record-type-descriptor &fault-condition) field) x)]))

  (define-syntax exit-reason (syntax-rules ()))
  (define-match-extension exit-reason
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(exit-reason always-match? r e)
         (with-temporaries (is-fault? tmp.reason)
           #`((bind is-fault? (fault-condition? #,v))
              (guard (or always-match? is-fault? (legacy-EXIT? #,v)))
              (bind tmp.reason
                (if is-fault?
                    (direct-&fault-condition-ref 'reason #,v)
                    (if always-match?
                        (if (legacy-EXIT? #,v)
                            (#3%vector-ref #,v 1)
                            #,v)
                        (#3%vector-ref #,v 1))))
              (handle-fields (#,v is-fault? tmp.reason) [reason r] [err e])))]))
    (lambda (input fld var options context)
      (syntax-case input ()
        [(v is-fault? tmp.reason)
         (case (syntax->datum fld)
           [(reason) #`((bind #,var tmp.reason))]
           [(err) #`((bind #,var (if (and is-fault? (direct-&fault-condition-ref 'k v)) v tmp.reason)))]
           [else (pretty-syntax-violation "unknown field" fld)])])))

  (define-match-extension catch
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(catch r)
         (with-temporaries (tmp)
           #`((sub-match #,v `(exit-reason #f r ,_))))]
        [`(catch r e)
         (with-temporaries (tmp)
           #`((sub-match #,v `(exit-reason #f r e))))])))

  (define process-parameter-ht (make-weak-eq-hashtable))

  (define make-process-parameter
    (case-lambda
     [(initial filter)
      (unless (procedure? filter)
        (bad-arg 'make-process-parameter filter))
      (let ([initial (filter initial)])
        (define v initial)
        (define parameter
          (case-lambda
           [() v]
           [(x) (set! v (filter x))]))
        (define (reset!) (set! v initial))
        (eq-hashtable-set! process-parameter-ht parameter reset!)
        parameter)]
     [(initial)
      (define v initial)
      (define parameter
        (case-lambda
         [() v]
         [(x) (set! v x)]))
      (define (reset!) (set! v initial))
      (eq-hashtable-set! process-parameter-ht parameter reset!)
      parameter]))

  (define (reset-process-parameters!)
    (vector-for-each (lambda (reset!) (reset!))
      (hashtable-values process-parameter-ht)))

  (record-writer (csv7:record-type-descriptor
                  (condition (make-error) (make-warning)))
    (lambda (x p wr)
      (display-string "#<compound condition: " p)
      (display-condition x p)
      (write-char #\> p)))

  (record-writer (record-type-descriptor &fault-condition)
    (lambda (r p wr)
      (display-string "#<fault " p)
      (wr (fault-condition-reason r) p)
      (write-char #\> p))))
