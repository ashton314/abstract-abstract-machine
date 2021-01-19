#lang racket

;; The Maybe Monad
(struct monad/maybe #:transparent)
(struct maybe/some monad/maybe (some-value) #:transparent)
(struct maybe/none monad/maybe #:transparent)

(define (return val)
  (maybe/some val))

(define (>>= v func)
  (match v
    [(maybe/some val) (func val)]
    [(maybe/none) v]))

(define ((ev ev) expr env)
  (match expr
    [(? symbol? var) (env/lookup env var)]
    [(? number? n) (return n)]
    [`(if0 ,e₀ ,e₁ ,e₂) (>>= (ev e₀ env)
                              (λ (c) (if (zero? c) (ev e₁ env) (ev e₂ env))))]
    ))
