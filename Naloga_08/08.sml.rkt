#lang racket

(struct zz (int) #:transparent)     ; cela števila
(struct true () #:transparent)        ; b ima lahko vrednost true or false
(struct false () #:transparent)
(struct add (e1 e2) #:transparent)  ; e1 in e2 sta izraza
(struct mul (e1 e2))
(struct leq? (e1 e2))
(struct ~ (e))
(struct is-zz? (e))
(struct if-then-else (e1 e2 e3))

(define-syntax ifte
  (syntax-rules (then else)
    [(ifte e1 then e2 else e3)
     (if-then-else e1 e2 e3)]))

(define-syntax geq?
  (syntax-rules ()
    [(geq? e1 e2)
     (leq? e2 e1)]))


(define (fri e)
  (cond [(zz? e) e]
        [(true? e) e]
        [(false? e) e]

        [(add? e)
         (let ([v1 (fri (add-e1 e))]
               [v2 (fri (add-e2 e))])
           (cond [(and (zz? v1) (zz? v2)) (zz (+ (zz-int v1) (zz-int v2)))]
                 [(and (true? v1) (or (true? v2) (false? v2))) (true)]
                 [(and (true? v2) (or (true? v1) (false? v1))) (true)]
                 [(and (false? v1) (false? v2)) (false)]
                 [#t (error "sintaksa izraza ni pravilna")]))]

        [(mul? e)
         (let ([v1 (fri (mul-e1 e))]
               [v2 (fri (mul-e2 e))])
           (cond [(and (zz? v1) (zz? v2)) (zz (* (zz-int v1) (zz-int v2)))]
                 [(and (false? v1) (or (true? v2) (false? v2))) (false)]
                 [(and (false? v2) (or (true? v1) (false? v1))) (false)]
                 [(and (true? v1) (true? v2)) (true)]
                 [#t (error "sintaksa izraza ni pravilna")]))]

        [(leq?? e)
         (let ([v1 (fri (leq?-e1 e))]
               [v2 (fri (leq?-e2 e))])
           (cond [(and (zz? v1) (zz? v2)) (if (<= (zz-int v1) (zz-int v2))
                                              (true)
                                              (false))]
                 [(and (true? v1) (false? v2)) (false)]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2))) (true)]                 
                 [#t (error "sintaksa izraza ni pravilna")]))]

        [(~? e)
         (let ([v (fri (~-e e))])
           (cond [(zz? v) (zz (-(zz-int v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (error "sintaksa izraza ni pravilna")]))]

        [(is-zz?? e)
         (let ([v (fri (is-zz?-e e))])
           (if (zz? v)
               (true)
               (false)))]

        [(if-then-else? e)
         (let ([v (fri (if-then-else-e1 e))])
           (if (true? v)
           (fri (if-then-else-e2 e))
           (fri (if-then-else-e3 e))))]
        
        [#t (error "sintaksa izraza ni pravilna")]
        ))