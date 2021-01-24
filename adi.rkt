#lang racket

(define (Y f)
  ((λ (x) (f (λ v (apply (x x) v))))
   (λ (x) (f (λ v (apply (x x) v))))))

(define ((aev aev) expr env store timestamp)
  42)

;; Nondeterminism
(define (return/nondet vals) vals)
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
        (Log v₂ (cons l₂ l₂))]
       [_ (error "Function broke contract in >>=/log")])]))
