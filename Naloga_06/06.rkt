#lang racket
(provide power gcd fib reverse remove map filter zip range is-palindrome)

(define (power x n)
  (if (eq? 0 n)
      1
      (* x (power x (- n 1)))))

(define (gcd a b)
  (if (eq? 0 b)
      a
      (gcd b (modulo a b))))

(define (fib n)
  (if (eq? 0 n)
      0
      (if (eq? 1 n)
          1
          (+ (fib (- n 1)) (fib (- n 2)))
       )
   )
)

(define (reverse lst)
  (if (null? lst)
      null
      (append
       (reverse (cdr lst))
       (list (car lst)))))

(define (remove x lst)
  (if (null? lst)
      null
      (if (eq? (car lst) x)
          (remove x (cdr lst))
          (cons (car lst) (remove x (cdr lst))))))

(define (map fn lst)
  (if (null? lst)
      null
      (cons (fn (car lst)) (map fn (cdr lst)))))

(define (filter fn lst)
  (if (null? lst)
      null
      (if (fn (car lst))
          (cons (car lst) (filter fn (cdr lst)))
          (filter fn (cdr lst)))))

(define (zip lst1 lst2)
  (cond [(or (null? lst1) (null? lst2)) null]
        [(cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))]))

(define (range a b korak)
  (if (<= a b)
      (cons a (range (+ a korak) b korak))
      null))

(define (is-palindrome lst)
  (letrec ([rev (reverse lst)]
           [palindrome (λ (lst rev)
                         (cond [(null? lst) #t]
                               [(eq? (car lst) (car rev)) (palindrome (cdr lst) (cdr rev))]
                               [#t #f]))])
    (palindrome lst rev)))
















