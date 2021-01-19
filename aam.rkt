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
    [`(if0 ,e₀ ,e₁ ,e₂) (>>= (ev e₀ env)
                              (λ (c) (if (zero? c) (ev e₁ env) (ev e₂ env))))]
    ))

;; The Maybe Monad
(struct monad/maybe () #:transparent)
(struct maybe/some monad/maybe (some-value) #:transparent)
(struct maybe/none monad/maybe () #:transparent)

(define (return val)
  (maybe/some val))

(define (>>= v func)
  (match v
    [(maybe/some val) (func val)]
    [(maybe/none) v]))

;; Environment
(define (fresh-env) (make-immutable-hash))

(define (env/lookup env var)
  ;; Woohoo! This is the first time I'm using call/cc in the wild!
  (call-with-current-continuation
   (λ (k) (return
           (hash-ref env var
                     (λ () (k (maybe/none))))))))

(define (env/ext env var val)
  (hash-set env var val))
