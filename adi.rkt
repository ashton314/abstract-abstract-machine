#lang racket

(define (Y f)
  ((λ (x) (f (λ v (apply (x x) v))))
   (λ (x) (f (λ v (apply (x x) v))))))

(define ((aev aev) expr env store timestamp)
  (match expr
    ;; Variable names
    [(? symbol? varname)
     (return/nondet
      (return/log
       (env/lookup env varname) 'var-lookup))]

    ;; Numbers
    [(? number? n)
     (return/nondet (return/log (return/maybe 'N) 'number-eval))]

    ;; Conditionals
    [`(if0 ,c ,e₁ ,e₂)
     (>>=/nondet
      (aev c env store timestamp)
      (λ (branch)
        (>>=/log
         branch
         (λ (maybe-val)
           (>>=/maybe
            maybe-val
            (λ (val)
              ;; At this point, we know that evaluating the
              ;; conditional along this branch has not blown up!
              (if (eq? val 'N)
                  ;; Might be a zero
                  (return/nondet
                   (return/log
                    (unstack (aev e₁ env store timestamp)
                             (λ (v) (return/nondet
                                     (return/log
                                      (return/maybe v) 'true-branch))))
                    'if-true-branch)
                   (return/log
                    (unstack (aev e₂ env store timestamp)
                             (λ (v) (return/nondet
                                     (return/log
                                      (return/maybe v) 'false-branch))))
                    'if-false-branch))
                  ;; Might have failed
                  (return/nondet (return/log (None) 'if-not-a-number))
                  )
              
              ))))))]
     
     ))

(define (unstack mmm f)
  (>>=/nondet mmm (λ (mm) (>>=/log mm (λ (m) (>>=/maybe m f))))))

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
(define (return/nondet vals)
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
