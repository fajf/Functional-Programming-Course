#lang racket

; TOKOVI

; cikla 1 2 3
(define one-two-three
  (letrec ([one
            (lambda (n) (cons n (thunk (one (+ (modulo n 3) 1)))))])
    (one 1)))

; cikla 1 2 3 na drugačen način
(define one-two-three*
  (letrec ([one
            (lambda (a b c) (cons a (thunk (one b c a))))])
    (one 1 2 3)))

; vrne n prvih števil toka t
(define (first n t)
  (if (zero? n)
      (list)
      (cons (car t)
            (first (sub1 n) ((cdr t))))))