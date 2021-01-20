#lang racket

;; (define (Y f)
;;   ((λ (x) (f (x x))) (λ (x) (f (x x)))))
(define (Y f)
  ((λ (x) (f (λ v (apply (x x) v))))
   (λ (x) (f (λ v (apply (x x) v))))))

(define ((fact f) n)
  (if (= n 1)
      1
      (* n (f (- n 1)))))

(define ((ev ev) expr env)
  (match expr
    [(? symbol? var) (env/lookup env var)]
    [(? number? n) (return n)]
    [`(if0 ,e₀ ,e₁ ,e₂)
     (>>= (ev e₀ env)
          (λ (c) (if (zero? c) (ev e₁ env) (ev e₂ env))))]
    [`(,(? primop? o) ,e₁ ,e₂)
     (>>= (ev e₁ env)
          (λ (v₁) (>>= (ev e₂ env)
                       (λ (v₂) (return ((find-op o) v₁ v₂))))))]
    [`(lam ,x ,e₀)
     (return (cons `(lam ,x ,e₀) env))]
    [`(app ,e₀ ,e₁)
     (>>= (ev e₀ env)
          (λ (closure)
            (>>= (ev e₁ env)
                 (λ (v)
                   (match closure
                     [(cons `(lam ,x ,body) closure-env)
                      (ev body (env/ext closure-env x v))])))))]))

(define (primop? sym)
  (member sym '(+ * / -)))

(define (find-op sym)
  (cdr (assoc sym (list (cons '+ +) (cons '* *) (cons '/ /) (cons '- -)))))

;; Example: ((Y ev) '(if0 (- (+ 3 4) 7) 1 0) (fresh-env))

;; The Maybe Monad
(struct monad/maybe () #:transparent)
(struct maybe/some monad/maybe (some-value) #:transparent)
(struct maybe/none monad/maybe (err) #:transparent)

(define (return val)
  (maybe/some val))

(define (>>= v func)
  (match v
    [(maybe/some val) (func val)]
    [(maybe/none _) v]))

;; Environment
(define (fresh-env) (make-immutable-hash))

(define (env/lookup env var)
  ;; Woohoo! This is the first time I'm using call/cc in the wild!
  (call-with-current-continuation
   (λ (k) (return
           (hash-ref env var
                     (λ () (k (maybe/none "no value matching in env"))))))))

(define (env/ext env var val)
  (hash-set env var val))

;; Store
(define fresh-store fresh-env)
(define store/lookup env/lookup)
(define store/ext env/ext)
