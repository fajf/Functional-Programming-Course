#lang racket

(define ones
  (cons 1 (lambda () ones)))

(define naturals
  (letrec ([naravna (lambda (n) (cons n (lambda () (naravna (+ n 1)))))])
    (naravna 1)))

(define fibs
  (letrec ([fib (lambda (a b) (cons (+ a b) (lambda () (fib b (+ a b)))))])
    (fib 1 0)))

(define (first n tok)
  (if (> n 0)
      (cons
        (car tok)
        (first (- n 1) ((cdr tok))))
      '()))

(define (squares tok)
  (cons
   (* (car tok) (car tok))
   (lambda () (squares ((cdr tok))))))


(define-syntax sml             ; ime makra
  (syntax-rules (:: hd tl null nil)       ; druge ključne besede
    [(sml a :: b)  ; sintaksa makra
     (list a b)]   ; razširitev makra
    [(sml hd lst)
     (car lst)]
    [(sml tl lst)
     (cdr lst)]
    [(sml nil)
     null]
    [(sml null lst)
     (null? lst)]))


(define-syntax my-delay
  (syntax-rules ()
    [(my-delay f)
     (mcons 0 (mcons null f))]))

(define-syntax my-force
  (syntax-rules ()
    [(my-force f)
     ((if (not (eq? 0 (modulo (mcar f) 5)))
          (begin (set-mcar! f (+ (mcar f) 1))
                 (lambda () (mcar (mcdr f))))
          (begin (set-mcar! f (+ (mcar f) 1))
                 (set-mcar! (mcdr f) ((mcdr (mcdr f))))
                 (lambda () (mcar (mcdr f))))))]))

(define (partitions k n)
  (if (eq? k 0)
         (if (eq? n 0)
             1                ; k in n sta oba 0
             0)               ; k je 0, n ni 0 
         (if (<= n 0)
             0                ; k ni 0, n je 0 ali manj
             (if (< k 0)
                 0            ; k je manj kot 0, n je pozitiven
                 (+
                  (partitions k (- n k))
                  (partitions (- k 1) (- n 1)))))           ; k in n sta oba pozitivna
         ))













