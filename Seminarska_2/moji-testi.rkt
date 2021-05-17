#lang racket
(require "02-project.rkt")




; testi
(require racket/trace)
;(trace fri)
;(untrace fri)

(define napacno 0)
(define (myequal? e1 e2)
  (let ([rezultat (equal? e1 e2)])
    (if rezultat
        #t
        (begin (println "================> FALSE") (set! napacno (+ napacno 1))))))


(myequal? (fri (qq (zz 4) (zz 2)) null) (qq (zz 2) (zz 1)))
(myequal? (fri (add (mul (true) (true)) (false)) null) (true))
;(myequal? (sestejRacionalna (qq (zz 1) (zz 3)) (qq (zz 1) (zz 5)) null) (qq (zz 8) (zz 15)))
;(myequal? (sestejRacionalna (qq (zz 2) (zz 1500)) (qq (zz 1) (zz 500)) null) (qq (zz 1) (zz 300)))
(myequal? (fri (qq (add (zz 1) (zz 3)) (add (zz 4) (zz 9))) null) (qq (zz 4) (zz 13)))
(myequal? (fri (empty) null) (empty))
(myequal? (fri (.. (zz 1) (zz 2)) null) (.. (zz 1) (zz 2)))
(myequal? (fri (.. (zz 1) (empty)) null) (.. (zz 1) (empty)))
(myequal? (fri (.. (zz 1) (.. (zz 2) (empty))) null) (.. (zz 1) (.. (zz 2) (empty))))
(myequal? (s (set (zz 1) (zz 2) (zz 1))) (s (set (zz 1) (zz 2) (zz 1))))
(myequal? (fri (s (set (zz 1) (zz 2) (zz 1))) null) (s (set (zz 1) (zz 2) (zz 1))))


(println "fri zz")
(myequal? (fri (zz 5) null) (zz 5))
(println "fri qq")
(myequal? (fri (qq (zz 4) (zz 6)) null) (qq (zz 2)(zz 3)))
(println "fri bool")
(myequal? (fri (true) null) (true))
(myequal? (fri (false) null) (false))
(println "fri seq")
(myequal? (fri (.. (zz 1) (empty)) null) (.. (zz 1) (empty)))
(myequal? (fri (.. (zz 1) (zz 2)) null) (.. (zz 1) (zz 2)))
(myequal? (fri (.. (zz 1) (qq (zz 3) (zz 9))) null) (.. (zz 1) (qq (zz 1) (zz 3))))
(myequal? (fri (.. (zz 1) (.. (zz 3) (zz 9))) null) (.. (zz 1) (.. (zz 3) (zz 9))))
(println "fri empty")
(myequal? (fri (empty) null) (empty))
(println "fri set")
(myequal? (fri (s (set (zz 1) (zz 2))) null) (s (set (zz 1) (zz 2))))
(myequal? (fri (s (set (.. (zz 1) (zz 2)))) null) (s (set (.. (zz 1) (zz 2)))))
(myequal? (fri (s (set (.. (zz 1) (zz 2)) (.. (zz 2) (zz 2)) (.. (zz 1) (zz 2)))) null)
        (s (set (.. (zz 1) (zz 2)) (.. (zz 2) (zz 2)))))

(println "fri is-zz?")
(myequal? (fri (is-zz? (zz 5)) null) (true))
(println "fri is-qq?")
(myequal? (fri (is-qq? (qq (zz 4) (zz 6))) null) (true))
(println "fri is-bool?")
(myequal? (fri (is-bool? (true)) null) (true))
(myequal? (fri (is-bool? (false)) null) (true))
(println "fri is-seq?")
(myequal? (fri (is-seq? (.. (zz 1) (empty))) null) (true))
(myequal? (fri (is-seq? (.. (zz 1) (zz 2))) null) (true))
(myequal? (fri (is-seq? (empty)) null) (false))
(println "fri is-proper-seq?")
(myequal? (fri (is-proper-seq? (.. (zz 1) (empty))) null) (true))
(myequal? (fri (is-proper-seq? (.. (zz 1) (zz 2))) null) (false))
(println "fri is-empty?")
(myequal? (fri (is-empty? (empty)) null) (true))
(myequal? (fri (is-empty? (zz 2)) null) (false))
(println "fri is-set?")
(myequal? (fri (is-set? (s (set (zz 1) (zz 2)))) null) (true))
(myequal? (fri (is-set? (empty)) null) (false))

(begin (println "============") (println "primerjajStevili") (println "============"))
#|
(myequal? (primerjajStevili (zz 1) (zz 2) null) (true))
(myequal? (primerjajStevili (zz 2) (zz 2) null) (true))
(myequal? (primerjajStevili (zz 3) (zz 2) null) (false))
(myequal? (primerjajStevili (zz 1) (qq (zz 2) (zz 1)) null) (true))
(myequal? (primerjajStevili (zz 1) (qq (zz 2) (zz 2)) null) (true))
(myequal? (primerjajStevili (zz 1) (qq (zz 2) (zz 3)) null) (false))
(myequal? (primerjajStevili (qq (zz 1) (zz 1)) (qq (zz 2) (zz 1)) null) (true))
(myequal? (primerjajStevili (qq (zz 1) (zz 1)) (qq (zz 2) (zz 2)) null) (true))
(myequal? (primerjajStevili (qq (zz 6) (zz 2)) (qq (zz 2) (zz 1)) null) (false))
|#

(begin (println "============") (println "fri add") (println "============"))
(myequal? (fri (add (qq (zz 1) (zz 2)) (add (zz 3) (zz 4))) null)
        (qq (zz 15) (zz 2)))

(myequal? (fri (add (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))) (.. (zz 4) (.. (zz 5) (.. (zz 6) (empty))))) null)
        (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 5) (.. (zz 6) (empty))))))))

(myequal? (fri (add (.. (zz 1) (.. (zz 2) (.. (zz 3) (zz 4)))) (.. (zz 4) (.. (zz 5) (zz 6)))) null)
        (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 4) (.. (zz 5) (zz 6))))))))

(myequal? (fri (add (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))) (.. (zz 4) (.. (zz 5) (zz 6)))) null)
        (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 4) (.. (zz 5) (zz 6)))))))

(myequal? (fri (add (s (set (zz 1) (zz 2))) (s (set (zz 2) (zz 3)))) null)
        (s (set (zz 3) (zz 1) (zz 2))))

(begin (println "============") (println "fri mul") (println "============"))
(myequal? (fri (mul (s (set (zz 1) (zz 2) (zz 3))) (s (set (true) (qq (zz 4) (zz 3))))) null)
        (s (set
            (.. (zz 2) (true))
            (.. (zz 2) (qq (zz 4) (zz 3)))
            (.. (zz 3) (true))
            (.. (zz 1) (qq (zz 4) (zz 3)))
            (.. (zz 3) (qq (zz 4) (zz 3)))
            (.. (zz 1) (true)))))
(myequal? (fri (mul (s (set (false) (true))) (mul (s (set (zz 1) (zz 2) (zz 3))) (s (set (zz 4) (zz 4))))) null)
        (s
         (set
          (.. (true) (.. (zz 1) (zz 4)))
          (.. (true) (.. (zz 3) (zz 4)))
          (.. (false) (.. (zz 2) (zz 4)))
          (.. (true) (.. (zz 2) (zz 4)))
          (.. (false) (.. (zz 1) (zz 4)))
          (.. (false) (.. (zz 3) (zz 4)))))) ; javni test

(begin (println "============") (println "fri leq?") (println "============"))
(myequal? (fri (leq? (zz 1) (zz 2)) null) (true))
(myequal? (fri (leq? (zz 2) (zz 2)) null) (true))
(myequal? (fri (leq? (zz 3) (zz 2)) null) (false))
(myequal? (fri (leq? (zz 1) (qq (zz 2) (zz 1))) null) (true))
(myequal? (fri (leq? (zz 1) (qq (zz 2) (zz 2))) null) (true))
(myequal? (fri (leq? (zz 1) (qq (zz 2) (zz 3))) null) (false))
(myequal? (fri (leq? (qq (zz 1) (zz 1)) (qq (zz 2) (zz 1))) null) (true))
(myequal? (fri (leq? (qq (zz 1) (zz 1)) (qq (zz 2) (zz 2))) null) (true))
(myequal? (fri (leq? (qq (zz 6) (zz 2)) (qq (zz 2) (zz 1))) null) (false))
(myequal? (fri (leq? (.. (zz 1) (empty)) (.. (zz 1) (zz 2))) null) (true))
(myequal? (fri (leq? (.. (zz 1) (zz 2)) (.. (zz 1) (empty))) null) (false))
(myequal? (fri (leq? (.. (zz 1) (zz 2)) (.. (zz 1) (zz 2))) null) (true))
(myequal? (fri (leq? (.. (zz 1) (.. (zz 2) (zz 3))) (.. (zz 1) (zz 2))) null) (false))
(myequal? (fri (leq? (.. (zz 1) (.. (zz 2) (empty))) (.. (zz 1) (zz 2))) null) (false))
(myequal? (fri (leq? (.. (zz 1) (.. (zz 2) (empty))) (.. (zz 1) (.. (zz 2) (zz 3)))) null) (true))
(myequal? (fri (leq? (.. (zz 1) (zz 2)) (.. (zz 1) (.. (zz 2) (zz 3)))) null) (true))
(myequal? (fri (leq? (.. (.. (true) (empty)) (zz 2)) (.. (zz 1) (zz 2))) null) (true))
(myequal? (fri (leq? (.. (zz 1) (.. (zz 2) (zz 3))) (.. (zz 1) (.. (zz 2) (empty)))) null) (false))
(myequal? (fri (leq? (s (set (zz 1) (zz 2))) (s (set (zz 1) (zz 2)))) null) (true))
(myequal? (fri (leq? (s (set (zz 1) (zz 2))) (s (set (zz 1) (zz 2) (zz 3)))) null) (true))
(myequal? (fri (leq? (s (set (zz 1) (zz 2))) (s (set (zz 1) (zz 3)))) null) (false))
(myequal? (fri (leq? (s (set (zz 1) (empty))) (s (set (zz 1) (zz 2) (zz 3)))) null) (false))
(myequal? (fri (leq? (s (set (zz 1) (empty))) (s (set (zz 1) (zz 2) (empty) (true)))) null) (true))

(begin (println "============") (println "fri rounding") (println "============"))
(myequal? (fri (rounding (zz 1)) null) (zz 1))
(myequal? (fri (rounding (qq (zz 1)(zz 2))) null) (zz 0))
(myequal? (fri (rounding (qq (zz 18)(zz 4))) null) (zz 4))
(myequal? (fri (rounding (qq (zz 22)(zz 4))) null) (zz 6))

(begin (println "============") (println "fri =?") (println "============"))
(myequal? (fri (=? (true) (true)) null) (true))
(myequal? (fri (=? (true) (false)) null) (false))
(myequal? (fri (=? (fri (add (mul (true) (true)) (false)) null) (true)) null) (true))
;(myequal? (fri (=? (sestejRacionalna (qq (zz 1) (zz 3)) (qq (zz 1) (zz 5)) null) (qq (zz 8) (zz 15))) null) (true))
;(myequal? (fri (=? (primerjajStevili (zz 1) (qq (zz 2) (zz 1)) null) (true)) null) (true))
(myequal? (fri (=? (fri (leq? (zz 1) (qq (zz 2) (zz 1))) null) (true)) null) (true))
(myequal? (fri (=? (fri (rounding (qq (zz 18)(zz 4))) null) (zz 4)) null) (true))

(begin (println "============") (println "fri left") (println "============"))
(myequal? (fri (left (qq (add (zz 1) (zz 3)) (zz 5))) null) (zz 4))
(myequal? (fri (left (.. (add (zz 1) (zz 2)) (.. (true) (false)))) null)
        (zz 3))
(myequal? (fri (left (s (set (zz 1) (zz 2) (empty)))) null)
        (s (empty)))

(begin (println "============") (println "fri right") (println "============"))
(myequal? (fri (right (qq (add (zz 1) (zz 3)) (zz 5))) null) (zz 5))
(myequal? (fri (right (.. (add (zz 1) (zz 2)) (.. (true) (false)))) null)
        (.. (true) (false)))
(myequal? (fri (right (s (set (zz 1) (zz 2) (empty)))) null)
        (s (set (zz 1) (zz 2))))

(begin (println "============") (println "fri ~") (println "============"))
(myequal? (fri (~ (zz 2)) null) (zz -2))
(myequal? (fri (~ (true)) null) (false))
(myequal? (fri (~ (false)) null) (true))
(myequal? (fri (~ (qq (zz 1) (zz 2))) null) (qq (zz -1)(zz 2)))

(begin (println "============") (println "fri all?") (println "============"))
(myequal? (fri (all? (.. (zz 1) (.. (zz 2) (true)))) null) (true))
(myequal? (fri (all? (.. (zz 1) (.. (false) (true)))) null) (false))
(myequal? (fri (all? (.. (zz 1) (.. (zz 2) (false)))) null) (false))
(myequal? (fri (all? (.. (false) (.. (zz 2) (true)))) null) (false))
(myequal? (fri (all? (.. (zz 1) (.. (add (false) (true)) (zz 2)))) null) (true))
(myequal? (fri (all? (.. (zz 1) (.. (mul (false) (true)) (zz 2)))) null) (false))
(myequal? (fri (all? (s (set (zz 1)(zz 2)(zz 3)))) null) (true))
(myequal? (fri (all? (s (set (zz 1)(zz 2)(false)))) null) (false))
(myequal? (fri (all? (s (set (zz 1)(false)(zz 3)))) null) (false))
(myequal? (fri (all? (s (set (false)(zz 2)(zz 3)))) null) (false))
(myequal? (fri (all? (s (set (add (true)(false))(zz 2)(zz 3)))) null) (true))
(myequal? (fri (all? (s (set (mul (true)(false))(zz 2)(zz 3)))) null) (false))

(begin (println "============") (println "fri any?") (println "============"))
(myequal? (fri (any? (.. (zz 1) (.. (zz 2) (true)))) null) (true))
(myequal? (fri (any? (.. (zz 1) (.. (false) (true)))) null) (true))
(myequal? (fri (any? (.. (zz 1) (.. (zz 2) (false)))) null) (true))
(myequal? (fri (any? (.. (false) (.. (zz 2) (true)))) null) (true))
(myequal? (fri (any? (.. (false) (.. (add (false) (true)) (false)))) null) (true))
(myequal? (fri (any? (.. (false) (.. (mul (false) (true)) (false)))) null) (false))
(myequal? (fri (any? (s (set (zz 1)(zz 2)(zz 3)))) null) (true))
(myequal? (fri (any? (s (set (zz 1)(zz 2)(false)))) null) (true))
(myequal? (fri (any? (s (set (zz 1)(false)(zz 3)))) null) (true))
(myequal? (fri (any? (s (set (false)(zz 2)(zz 3)))) null) (true))
(myequal? (fri (any? (s (set (add (true)(false))(false)(false)))) null) (true))
(myequal? (fri (any? (s (set (mul (true)(false))(false)(false)))) null) (false))

(begin (println "============") (println "SPREMENLJIVKE") (println "============"))
(myequal? (fri (valof "a") (list (cons "a" (zz 5)))) (zz 5))
(myequal? (fri (vars "a" (zz 1) (valof "a")) (list (cons "a" (zz 5)))) (zz 1))
(myequal? (fri (add (vars "a" (zz 1) (valof "a")) (valof "a")) (list (cons "a" (zz 5)))) (zz 6))
(myequal? (fri (add (valof "a") (vars "a" (zz 1) (valof "a"))) (list (cons "a" (zz 5)))) (zz 6))
(myequal? (fri (vars (list "a" "b") (list (zz 1) (zz 2)) (add (valof "a") (valof "b"))) (list (cons "a" (zz 5)))) (zz 3))

(begin (println "============") (println "FUNKCIJE") (println "============"))
(myequal?
 (fri (vars (list "a" "b" "c")
            (list (zz 1) (zz 2) (zz 3))
            (fun "linear" (list "x1" "x2" "x3")
                 (add (mul (valof "a") (valof "x1"))
                      (add (mul (valof "b") (valof "x2"))
                           (mul (valof "c") (valof "x3")))))) null)
 (closure (list (cons "a" (zz 1))(cons "b" (zz 2)) (cons "c" (zz 3)))
          (fun "linear" '("x1" "x2" "x3")
               (add (mul (valof "a") (valof "x1"))
                    (add (mul (valof "b") (valof "x2"))
                         (mul (valof "c") (valof "x3"))))))) ; javni test

(myequal?
 (fri (fun "fib" (list "n")
            (if-then-else (leq? (valof "n") (zz 2))
                          (zz 1) (add (call (valof "fib")
                                            (list (add (valof "n") (zz -1))))
                                      (call (valof "fib")
                                            (list (add (valof "n") (zz -2))))))) null)
 (closure '()
          (fun "fib" '("n")
               (if-then-else
                (leq? (valof "n") (zz 2))
                (zz 1)
                (add
                 (call (valof "fib") (list (add (valof "n") (zz -1))))
                 (call (valof "fib") (list (add (valof "n") (zz -2)))))))))

(myequal?
 (fri (call (fun "a" null (add (zz 1) (zz 2))) null) null)
 (zz 3))

(myequal?
 (fri (call
       (fun "a" (list "b") (add (zz 1) (valof "b")))
       (list (zz 2)))
      null)
 (zz 3))

(myequal?
 (fri (call (fun "a" (list "b") (add (zz 1) (valof "b"))) (list (zz 2))) (list (cons "b" 5)))
 (zz 3))

(myequal?
 (fri (vars (list "b" "c")
            (list (zz 5) (zz 3))
            (call
             (fun "a" (list "b") (add (zz 1) (valof "b")))
             (list (zz 2)))) null)
 (zz 3))

(myequal?
 (fri (vars (list "b" "c")
            (list (zz 5) (fun "a" (list "b") (add (zz 1) (valof "b"))))
            (call
             (valof "c")
             (list (zz 2)))) null)
 (zz 3))

(myequal?
 (fri (vars (list "b")
            (list (zz 5))
            (vars (list "c")
                  (list (fun "a" null (add (zz 1) (valof "b"))))
                  (vars (list "b")
                        (list (zz 10))
                        (call (valof "c") null))))
            null)
 (zz 6))

(println "Anonimne funkcije")
(myequal?
 (fri (call (fun "" null (add (zz 1) (zz 2))) null) null)
 (zz 3))

(myequal?
 (fri (call (fun "" (list "a" "b") (add (valof "a") (valof "b"))) (list (zz 1)(zz 2))) null)
 (zz 3))

(println "Rekurzivne funkcije:")
(myequal?
 (fri (call
       (fun "sestevaj" (list "x" "acc")
            (if-then-else (leq? (valof "x") (zz 0))
                          (valof "acc")
                          (call
                           (valof "sestevaj")
                           (list (add (valof "x") (zz -1)) (add (valof "acc") (valof "x"))))))
       (list (zz 5) (zz 0)))
      null)
 (zz 15))

(myequal?
 (fri (call
       (fun "inverz" (list "seznam")
            (if-then-else (is-seq? (valof "seznam"))
                          (vars "preostanek"
                                (right (valof "seznam"))
                                (if-then-else (is-seq? (valof "preostanek"))
                                              (call (valof "inverz") (list (..
                                                                            (.. (left (valof "preostanek")) (left (valof "seznam")))
                                                                            (right (valof "preostanek")))))
                                              (..
                                               (valof "preostanek")
                                               (left (valof "seznam")))))
                          (valof "seznam")))
       (list (.. (zz 0) (.. (zz 1) (.. (zz 2) (zz 3)))))) null)
 (.. (zz 3) (.. (zz 2) (.. (zz 1) (zz 0)))))
                                

; javni test fib - rekurzija
(myequal?
 (fri (call (fun "fib" (list "n")
                (if-then-else (leq? (valof "n") (zz 2))
                              (zz 1)
                              (add (call (valof "fib")
                                         (list (add (valof "n") (zz -1))))
                                   (call (valof "fib")
                                         (list (add (valof "n") (zz -2)))))))
           (list (zz 10)))
     null)
 (zz 55))

(begin (println "============") (println "PROCEDURE") (println "============"))
(myequal?
 (fri (proc "a" (add (zz 1) (zz 2))) null)
 (proc "a" (add (zz 1) (zz 2))))

(myequal?
 (fri (call (proc "a" (add (zz 1) (zz 2))) null) null)
 (zz 3))

(myequal?
 (fri (vars (list "a" "b")
            (list (zz 1) (proc "a" (add (zz 1) (zz 2))))
            (call (valof "b") null))
      null)
 (zz 3))

(myequal?
 (fri (vars (list "a" "b")
            (list (zz 1) (proc "c" (add (valof "a") (zz 2))))
            (call (valof "b") null))
      null)
 (zz 3))

(println "Rekurzivne procedure:")
(myequal?
 (fri (vars (list "x" "acc")
            (list (zz 5) (zz 0))
            (call (proc "sestevaj"
                        (if-then-else
                         (leq? (valof "x") (zz 0))
                         (valof "acc")
                         (vars (list "x" "acc")
                               (list (add (valof "x") (zz -1)) (add (valof "acc") (valof "x")))
                               (call (valof "sestevaj") null))))
                  null))
      null)
 (zz 15))

(begin (println "============") (println "MAKRO SISTEM") (println "============"))
(myequal? (fri (numerator (qq (zz 1) (zz 2))) null) (zz 1))
(myequal? (fri (denominator (qq (zz 1) (zz 2))) null) (zz 2))
(myequal? (fri (gt? (zz 1) (zz 2)) null) (true))
(myequal? (fri (inv (qq (zz 1) (zz 2))) null) (qq (zz 2) (zz 1)))
(myequal? (fri (inv (zz 2)) null) (qq (zz 1) (zz 2)))
(myequal? (fri (inv (.. (zz 1) (.. (zz 2) (.. (zz 3) (zz 4))))) null) (.. (zz 4) (.. (zz 3) (.. (zz 2) (zz 1)))))

(println (~a "Število neuspešnih testov: " napacno))
