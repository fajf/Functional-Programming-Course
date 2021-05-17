#lang racket
(require "06.rkt")

(display "Reverse 1 ")
(displayln (equal?
            (reverse (list 1 2 3))
            (list 3 2 1)))

(display "Remove 1 ")
(displayln (equal?
            (remove 0 (list 0 1 2 3 4 5 0 1 2 3 4 5))
            (list 1 2 3 4 5 1 2 3 4 5)))

(display "Remove 2 ")
(displayln (equal?
            (remove 5 (list 0 1 2 3 4 5 0 1 2 3 4 5))
            (list 0 1 2 3 4 0 1 2 3 4)))

(display "Map 1 ")
(displayln (equal?
            (map (λ (a) (* a 2)) (list 0 1 2 3 4 5 0 1 2 3 4 5))
            (list 0 2 4 6 8 10 0 2 4 6 8 10)))

(display "Filter 1 ")
(displayln (equal?
            (filter (λ (a) (< a 3)) (list 0 1 2 3 4 5 0 1 2 3 4 5))
            (list 0 1 2 0 1 2)))

(display "Zip 1 ")
(displayln (equal?
            (zip (list 1 2 3) (list 4 5 6))
            (list (cons 1 4) (cons 2 5) (cons 3 6))))


(display "Zip 1 ")
(displayln (equal?
            (zip (list 1 2 3 8 9) (list 4 5 6 7 0))
            (list (cons 1 4) (cons 2 5) (cons 3 6) (cons 8 7) (cons 9 0))))

(display "Range 1 ")
(displayln (equal?
            (range 0 10 2)
            (list 0 2 4 6 8 10)))

(display "Range 2 ")
(displayln (equal?
            (range 0 11 2)
            (list 0 2 4 6 8 10)))

(display "Range 3 ")
(displayln (equal?
            (range 5 10 2)
            (list 5 7 9)))

(display "Palindrome 1 ")
(displayln (equal?
            (is-palindrome (list 2 3 3 2))
            #t))

(display "Palindrome 2 ")
(displayln (equal?
            (is-palindrome (list 2 3 5 1 6 1 5 3 2))
            #t))