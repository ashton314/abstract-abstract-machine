#lang racket

;; THE MIGHTY Y-COMBINATOR!!!
(define (Y f)
  ((λ (x) (f (λ v (apply (x x) v))))
   (λ (x) (f (λ v (apply (x x) v))))))

;; Caches
(define $in (make-hash))
(define $out (make-hash))

(define (clear-caches!)
  (set! $in (make-hash))
  (set! $out (make-hash)))

(define (inspect label v)
  (display label)
  (displayln v)
  v)

;; Caching evaluator
;; Call like ((Y aev-cache) '(+ 1 2) (fresh-env) '() '())
(define ((aev-cache aev-cache) expr env store timestamp)
  (flatten
   (set->list
    (hash-ref
     $out (list expr env)                 ; If the config is in $out, use it
     (λ ()
       ;; Otherwise, create an empty entery in $in
       (let ([in-set-val (hash-ref $in (list expr env) (mutable-set))])
         (hash-set! $out (list expr env) in-set-val)
         (let ([out ((aev aev-cache) expr env store timestamp)])
           (hash-update!
            $out
            (list expr env)
            (λ (out-vals)
              (set-add! out-vals out)
              out-vals))
           out)))))))

;; Evaluator
(define ((aev aev) expr env store timestamp)
  (match expr
    ;; Variable names
    [(? symbol? varname)
     (return/nondet
      (return/log
       (env/lookup env varname) null))]

    ;; Numbers
    [(? number? n)
     (return/nondet (return/log (return/maybe 'N) null))]

    ;; Conditionals
    [`(if0 ,c ,e₁ ,e₂)
     (stack
      (aev c env store timestamp)
      (λ (cv)
        (if (eq? cv 'N)
            (return/nondet
             (stack (aev e₁ env store timestamp)
                    (λ (v₁) (return/nondet (return/log (Some v₁) null))))
             (stack (aev e₂ env store timestamp)
                    (λ (v₂) (return/nondet (return/log (Some v₂) null)))))
            (return/nondet (return/log (None) null))
            )))]

    ;; Arithmetic
    [`(,(? safe-primop? o) ,e₁ ,e₂)
     (stack
      (aev e₁ env store timestamp)
      (λ (v₁)
        (stack
         (aev e₂ env store timestamp)
         (λ (v₂)
           (return/nondet
            (return/log
             (if (eq? v₁ 'N)
                 (if (eq? v₂ 'N)
                     (Some 'N)
                     (None))
                 (None)) o))))))]
    [`(,(? unsafe-primop? o) ,e₁ ,e₂)
     (stack
      (aev e₁ env store timestamp)
      (λ (v₁)
        (stack
         (aev e₂ env store timestamp)
         (λ (v₂)
           (return/nondet
            (return/log
             (if (eq? v₁ 'N)
                 (if (eq? v₂ 'N)
                     (Some 'N)
                     (None))
                 (None)) o)
            (return/log (None) 'divide-by-zero))))))]

    ;; Lambda: The Ultimate Paper Topic
    [`(lam ,x ,e₀)
     (return/nondet
      (return/log
       (return/maybe (cons `(lam ,(gensym 'λ) ,x ,e₀) env)) null))]

    [`(app ,e₀ ,e₁)
     (stack
      (aev e₀ env store timestamp)
      (λ (closure)
        (stack
         (aev e₁ env store timestamp)
         (λ (val)
           (match closure
             [(cons `(lam ,loc ,x ,body) closure-env)
              (aev body (env/ext closure-env x val) store timestamp)])))))]

    ))

(define (safe-primop? sym)
  (member sym '(+ * -)))
(define (unsafe-primop? sym)
  (member sym '(/)))


;; This is kind of like a composition ofo the various monads that I have. It's... ugly.
(define (stack mmm f)
  (flatten
   (map
    (λ (branch)
      ;; A branch is Log(Maybe, ())
      (match branch
        [(Log mv l)
         (match mv
           [(Some v)

            (map                         ; over returned non-det
             (λ (new-branch)
               (match new-branch
                 [(Log bv l₂) (Log bv (flatten (if l₂ (cons l₂ l) l)))]))
             (f v))

            ]
           [(None) branch])
         ]))
    mmm)))

;; (define ((ndet-ev ndet-ev) expr-set env timestamp)
;;   (>>=/nondet
;;    expr-set
;;    (λ (branch)
;;      (>>=/log
;;       branch
;;       (λ (?val)
        
;;         ))))

;; Environment
(define (fresh-env) (make-immutable-hash))

(define (env/lookup env var)
  ;; Woohoo! This is the first time I'm using call/cc in the wild!
  (call-with-current-continuation
   (λ (k) (return/maybe (hash-ref env var (λ () (k (None))))))))

(define (env/ext env var val)
  (hash-set env var val))

;; Nondeterminism
(define (return/nondet . vals)
  (flatten vals))                       ; if given a single value, puts it into a list
(define (>>=/nondet m f)
  (flatten (map f m)))

;; Failure
(struct Maybe () #:transparent)
(struct Some Maybe (val) #:transparent)
(struct None Maybe () #:transparent)

(define (return/maybe val) (Some val))
(define (>>=/maybe v f)
  (match v
    [(Some val) (f val)]
    [(None) v]))

;; Logging
(struct Log (val log) #:transparent)

(define (return/log val log) (Log val log))
;; f :: A -> Log(B, _)
(define (>>=/log m f)
  (match m
    [(Log v₁ l₁)
     (match (f v₁)
       [(Log v₂ l₂)
        (Log v₂ (if l₂ (cons l₂ l₁) l₁))]
       [_ (error "Function broke contract in >>=/log")])]))
