#lang racket
; da ne izgubimo funkcij `numerator` in `denominator` zaradi "naših" makrojev 
(require (rename-in racket (numerator qnumerator)
                    (denominator qdenominator)))

(provide false true zz qq .. empty s
         if-then-else
         is-zz? is-qq? is-bool? is-seq? is-proper-seq? is-empty? is-set?
         add mul leq? rounding =? right left ~ all? any?
         vars valof fun proc closure call
         gt? inv numerator denominator filtering folding mapping
         fri)

;==============================================================================================

(struct true () #:transparent)     
(struct false () #:transparent)
(struct zz (int) #:transparent)     ; cela števila
(struct qq (e1 e2) #:transparent)   ; racionalna števila
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct s (e) #:transparent)

(struct if-then-else (e1 e2 e3) #:transparent)

(struct is-zz? (e) #:transparent)
(struct is-qq? (e) #:transparent)
(struct is-bool? (e) #:transparent)
(struct is-seq? (e) #:transparent)
(struct is-proper-seq? (e) #:transparent)    ; rezultat je (true) le, če je podano zaporedje "pravo"
                               ; zaporedje (se konča z (empty))
(struct is-empty? (e) #:transparent)
(struct is-set? (e) #:transparent)


(struct add (e1 e2) #:transparent)  ; e1 in e2 sta izraza
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct rounding (e) #:transparent)
(struct =? (e1 e2) #:transparent)
(struct left (e) #:transparent)
(struct right (e) #:transparent)
(struct ~ (e) #:transparent)
(struct all? (e) #:transparent)
(struct any? (e) #:transparent)

(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)
; ==========================================================================

; =================================
; POMOŽNE FUNKCIJE
; =================================

(define (okrajsajUlomek e env)
  (cond
    [(qq? e)
     (let* ([v1 (zz-int (fri (qq-e1 e) env))]
            [v2 (zz-int (fri (qq-e2 e) env))]
            [nsd (gcd v1 v2)])
       (qq (zz (/ v1 nsd)) (zz (/ v2 nsd))))]
    
    [#t (error "sintaksa izraza ni pravilna")]))

    
(define (sestejRacionalna e1 e2 env)
  (cond
    [(and (qq? e1) (qq? e2))  ; obe stevili sta ulomka
     (let* ([prvo (/ (zz-int (fri (qq-e1 e1) env)) (zz-int (fri (qq-e2 e1) env)))]
            [drugo (/ (zz-int (fri (qq-e1 e2) env)) (zz-int (fri (qq-e2 e2) env)))]
            [sestevek (+ prvo drugo)])
       (okrajsajUlomek (qq (zz (qnumerator sestevek)) (zz (qdenominator sestevek))) env))]
    [(and (qq? e1) (zz? e2))  ; prvo stevilo je ulomek, drugo je celo
     (let* ([prvo (/ (zz-int (fri (qq-e1 e1) env)) (zz-int (fri (qq-e2 e1) env)))]
            [drugo (/ (zz-int e2) 1)]
            [sestevek (+ prvo drugo)])
       (okrajsajUlomek (qq (zz (qnumerator sestevek)) (zz (qdenominator sestevek))) env))]
    [(and (qq? e1) (zz? e2))  ; prvo stevilo je celo, drugo je ulomek
     (let* ([drugo (/ (zz-int (fri (qq-e1 e2) env)) (zz-int (fri (qq-e2 e2) env)))]
            [prvo (/ (zz-int e1) 1)]
            [sestevek (+ prvo drugo)])
       (okrajsajUlomek (qq (zz (qnumerator sestevek)) (zz (qdenominator sestevek))) env))]
    [#t (error "sintaksa izraza ni pravilna")]))

(define (zmnoziRacionalna e1 e2 env)
  (let* ([prvo (/ (zz-int (fri (qq-e1 e1) env)) (zz-int (fri (qq-e2 e1) env)))]
         [drugo (/ (zz-int (fri (qq-e1 e2) env)) (zz-int (fri (qq-e2 e2) env)))]
         [zmnozek (* prvo drugo)])
    (okrajsajUlomek (qq (zz (qnumerator zmnozek)) (zz (qdenominator zmnozek))) env)))

(define (primerjajStevili e1 e2 env)   ; stevili sta lahko tipa zz ali qq (ne nujno obe istega tipa)
  (cond
    [(and (zz? e1) (zz? e2))
     (let ([prvo (zz-int e1)]
           [drugo (zz-int e2)])
       (if (<= prvo drugo)
            (true)
            (false)))]
    [(and (zz? e1) (qq? e2))
     (let ([prvo (zz-int e1)]
           [drugo (/ (zz-int (qq-e1 e2)) (zz-int (qq-e2 e2)))])
       (if (<= prvo drugo)
            (true)
            (false)))]
    [(and (qq? e1) (zz? e2))
     (let ([prvo (/ (zz-int (qq-e1 e1)) (zz-int(qq-e2 e1)))]
           [drugo (zz-int e2)])
       (if (<= prvo drugo)
            (true)
            (false)))]
    [(and (qq? e1) (qq? e2))
     (let ([prvo (/ (zz-int(qq-e1 e1)) (zz-int(qq-e2 e1)))]
           [drugo (/ (zz-int(qq-e1 e2)) (zz-int(qq-e2 e2)))])
       (if (<= prvo drugo)
            (true)
            (false)))]))

(define (sestejZaporedji e1 e2 env)
  (cond
    [(..? e1)
     (let ([v1 (fri (..-e1 e1) env)]
           [v2 (fri (..-e2 e1) env)])
       (cond [(..? v2)   ; drugi element nadaljuje zaporedje
              (.. v1 (sestejZaporedji v2 e2 env))]
             [(empty? v2)   ; drugi elemente zakljucuje pravo zaporedje
              (if (null? e2)     ; ali je to že drugi seznam?
                  (.. v1 v2)  ; DA - zaključimo
                  (.. v1 (sestejZaporedji e2 null  env)))]   ; NE - nadaljujemo z drugim seznamom
             [#t   ; drugi element zakljucuje zaporedje, vendar to ni pravo zaporedje
              (if (null? e2)     ; ali je to že drugi seznam?
                  (.. v1 v2)  ; DA - zaključimo
                  (.. v1 (.. v2 (sestejZaporedji e2 null  env))))]; NE - nadaljujemo z drugim seznamom
             ))]
    [(empty? e1)
     (if (null? e2)     ; ali je to že drugi seznam?
         e1  ; DA - zaključimo
         (sestejZaporedji e2 null env))]   ; NE - nadaljujemo z drugim seznamom
    ))   

(define (listPar->.. lst)
  (.. (car lst) (car (cdr lst))))

(define (dodajVOkolje env s v)
  (cond
    [(null? v)
     env]
    [(list? v)
     (append env (map cons s (map (lambda (e) (fri e env)) v)))]
    [(not (list? v))
     (append env (list (cons s (fri v env))))]
    [#t (error "sintaksa izraza ni pravilna")]))

; =================================
; MAKRO SISTEM
; =================================

(define (numerator e1)
  (left e1))

(define (denominator e1)
  (right e1))

(define (gt? e1 e2)
  (leq? e1 e2))

(define (inv e1)
  (if-then-else (is-zz? e1)
                (qq (zz 1) e1)
                (if-then-else (is-qq? e1)
                              (qq (right e1) (left e1))
                              (if-then-else (is-seq? e1)  ; če je seznam, kličem pomožno funkcijo
                                            (call (fun "inverz" (list "seznam")
                                                              (if-then-else (is-seq? (valof "seznam")) ; ali je argument klica seznam?
                                                                            (vars "preostanek" ; če je, shrani preostanek zaporedja
                                                                                  (right (valof "seznam"))
                                                                                  (if-then-else (is-seq? (valof "preostanek"))  ; ali je preostanek tudi zaporedje?
                                                                                                (call (valof "inverz") (list (..  ; če je, kliči pomožno funkcijo rekurzivno, modificiraj prvo mesto seznama
                                                                                                                              (.. (left (valof "preostanek")) (left (valof "seznam")))
                                                                                                                              (right (valof "preostanek")))))
                                                                                                (.. ; če preostanek ni zaporedje, smo prišli do konca in le še drugi element postavi na prvo mesto
                                                                                                 (valof "preostanek")
                                                                                                 (left (valof "seznam")))))
                                                                            (valof "seznam"))) ; če argument ni seznam, zgolj vrni vrednost
                                                         (list e1))
                                             
                                            (thunk (error "Vrednost ni celo število, racionalno število ali zaporedje"))))))

(define (mapping f seq)
  (error "Not implemented"))

(define (filtering f seq)
  (error "Not implemented"))

(define (folding f init seq)
  (error "Not implemented"))


; =================================
; FRI
; =================================

(define (fri e env)
  (cond
    ; =================================
    ; PODATKOVNI TIPI
    ; =================================
    [(true? e) e]
    [(false? e) e]
        
    [(zz? e) e]
        
    [(qq? e)
     (okrajsajUlomek e env)]
        
    [(..? e)
     (let* ([v1 (fri (..-e1 e) env)]
            [v2 (fri (..-e2 e) env)])
       (.. v1 v2))]

    [(empty? e) e]

    [(s? e)
     (let ([v (s-e e)])
       (s (list->set (set-map v (lambda (e) (fri e env))))))]

    ; =================================
    ; NADZOR TOKA
    ; =================================
        
    ; VEJITEV
    [(if-then-else? e)
     (let* ([v (fri (if-then-else-e1 e) env)])
       (if (false? v)
           (fri (if-then-else-e3 e) env)
           (fri (if-then-else-e2 e) env)))]

    ; PREVERJANJE TIPOV
    [(is-zz?? e)
     (let ([v (fri (is-zz?-e e) env)])
       (if (zz? v)
           (true)
           (false)))]

    [(is-qq?? e)
     (let ([v (fri (is-qq?-e e) env)])
       (if (qq? v)
           (true)
           (false)))]

    [(is-bool?? e)
     (let ([v (fri (is-bool?-e e) env)])
       (if (or (true? v) (false? v))
           (true)
           (false)))]

    [(is-seq?? e)
     (let ([v (fri (is-seq?-e e) env)])
       (if (..? v)
           (true)
           (false)))]

    [(is-proper-seq?? e)
     (let ([v (fri (is-proper-seq?-e e) env)])
       (cond [(..? v)
              (let ([v1 (fri (..-e1 v) env)]
                    [v2 (fri (..-e2 v) env)])
                (if (empty? v2)
                    (true)
                    (fri (is-proper-seq? v2) env)))]
             [#t (false)]))]
    
    [(is-empty?? e)
     (let* ([v (fri (is-empty?-e e) env)])
       (if (empty? v)
           (true)
           (false)))]
    
    [(is-set?? e)
     (let ([v (fri (is-set?-e e) env)])
       (if (s? v)
           (let ([vv (s-e v)])
             (if (set? vv)
                 (true)
                 (false)))
           (false)))]

    ; SEŠTEVANJE
    [(add? e)
     (let* ([v1 (fri (add-e1 e) env)]
            [v2 (fri (add-e2 e) env)])
       (cond
         ; Če sta izraza e1 in e2 celi števili, potem je rezultat njuna vsota, ki je spet celo število.
         [(and (zz? v1) (zz? v2)) (zz (+ (zz-int v1) (zz-int v2)))]
         ; Če sta izraza logični vrednosti je rezultat disjunkcija
         [(and (true? v1) (or (true? v2) (false? v2))) (true)]
         [(and (true? v2) (or (true? v1) (false? v1))) (true)]
         [(and (false? v1) (false? v2)) (false)]
         ; Če je eden izmed izrazov e1 in e2 racionalno število (in je drugi izraz celo ali
         ; racionalno število), potem je rezultat racionalno število, ki predstavlja njuno vsoto.
         [(and (or (qq? v1) (zz? v1)) (or (qq? v2) (zz? v2)))
          (sestejRacionalna v1 v2 env)]
         ; Če sta izraza e1 in e2 zaporedji, je rezultat seštevanja njuna združitev, tako da zaporedje e1 nadaljujemo z e2
         [(and (or (..? v1) (empty? v1)) (or (..? v2) (empty? v2)))
          (sestejZaporedji v1 v2  env)]
         ; Če sta izraz e1 in e2 množici, je rezultat njuna unija
         [(and (s? v1) (s? v2))
          (let ([vv1 (s-e v1)]
                [vv2 (s-e v2)])
            (s (set-union vv1 vv2)))] ; vrstni red unije je čuden
         
         [#t (error "sintaksa izraza ni pravilna")]))]

    ; MNOŽENJE
    [(mul? e)
     (let* ([v1 (fri (mul-e1 e) env)]
            [v2 (fri (mul-e2 e) env)])
       (cond
         ; Če sta izraza e1 in e2 celi števili, potem je rezultat njun produkt, ki je spet celo število.
         [(and (zz? v1) (zz? v2)) (zz (* (zz-int v1) (zz-int v2)))]
         ; Če sta izraza logični vrednosti je rezultat konjunkcija
         [(and (false? v1) (or (true? v2) (false? v2))) (false)]
         [(and (false? v2) (or (true? v1) (false? v1))) (false)]
         [(and (true? v1) (true? v2)) (true)]
         ; Če je eden izmed izrazov e1 in e2 racionalno število (in je drugi izraz celo ali racionalno
         ; število), potem je rezultat racionalno število, ki predstavlja njun produkt.
         [(and (or (qq? v1) (zz? v1)) (or (qq? v2) (zz? v2)))
          (zmnoziRacionalna v1 v2 env)]
         ; Če sta izraza množici je rezultat njun katarzični produkt
         [(and (s? v1) (s? v2))
          (let* ([vv1 (set->list (s-e v1))]
                 [vv2 (set->list (s-e v2))]
                 [produkt (cartesian-product vv1 vv2)])
            (s (list->set (set-map produkt listPar->..))))]

         [#t (error "sintaksa izraza ni pravilna")]))]
    
    ;PRIMERJANJE
    [(leq?? e)
     (let* ([v1 (fri (leq?-e1 e) env)]
            [v2 (fri (leq?-e2 e) env)])
       (cond
         ; Če sta izraza e1 in e2 celi ali racionalni števili, potem je rezultat e1 <= e2
         [(and (or (zz? v1) (qq? v1)) (or (zz? v2) (qq? v2)))
          (primerjajStevili v1 v2 env)]
         ; Če sta izraza logični vrednosti je rezultat implikacija e1 => e2
         [(and (true? v1) (false? v2)) (false)]
         [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2))) (true)]
         ; Če sta izraza zaporedji je rezultat (true), če ima zaporedje e1 enako ali manjše število elementov kot zaporedje e2
         [(and (..? v1) (..? v2))
          (let ([drugi1 (fri (..-e2 v1) env)]
                [drugi2 (fri (..-e2 v2) env)])
            (cond
              [(and (..? drugi1) (not (..? drugi2))) (false)]
              [(and (not (empty? drugi1)) (empty? drugi2)) (false)]
              [(and (..? drugi1) (..? drugi2)) (fri (leq? drugi1 drugi2) env)]
              [#t (true)]))]
         ; Če sta izraza množici je rezultat (true), če množica e1 podmnožica množice e2
         [(and (s? v1) (s? v2))
          (if (subset? (s-e v1) (s-e v2))
              (true)
              (false))]
         [#t (error "sintaksa izraza ni pravilna")]))]

    ; ZAOKROŽEVANJE
    [(rounding? e)
     (let ([v (fri (rounding-e e) env)])
       (cond
         [(zz? v) v]
         [(qq? v)
          (zz (round (/ (zz-int (qq-e1 v)) (zz-int (qq-e2 v)))))]
         [#t (error "sintaksa izraza ni pravilna")]))]

    ; UJEMANJE
    [(=?? e)
     (let ([v1 (fri (=?-e1 e) env)]
           [v2 (fri (=?-e2 e) env)])
       (if (equal? v1 v2)
           (true)
           (false)))]

    ; EKSTRAKCIJA
    [(left? e)
     (let ([v (fri (left-e e) env)])
       (cond
         [(qq? v) (qq-e1 v)]
         ; Za zaporedje e1 izraz (left e1) vrne prvi element zaporedja
         [(..? v) (..-e1 v)]
         ; Za množico e1 izraz (left e1) vrne "prvi" element množice — uporablja set-first
         [(s? v) (s (set-first (s-e v)))]
         [#t (error "sintaksa izraza ni pravilna")]))]

    [(right? e)
     (let ([v (fri (right-e e) env)])
       (cond
         [(qq? v) (qq-e2 v)]
         ; Za zaporedje e1 izraz (right e1) vrne preostali del zaporedja.
         [(..? v) (..-e2 v)]
         ; Za množico e1 izraz (right e1) vrne preostali del množice — uporablja set-rest
         [(s? v) (s (set-rest (s-e v)))]
         [#t (error "sintaksa izraza ni pravilna")]))]

    ; NASPROTNA VREDNOST
    [(~? e)
         (let ([v (fri (~-e e) env)])
           (cond [(zz? v) (zz (-(zz-int v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [(qq? v)
                  (qq
                   (zz (-(zz-int (qq-e1 v))))
                   (qq-e2 v))]
                 [#t (error "sintaksa izraza ni pravilna")]))]

    ; Če zaporedje (ali množica) e1 ne vsebuje logične vrednosti (false), potem je rezultat izraza (all? e1) enak (true), drugače (false)
    [(all?? e)
     (let ([v (fri (all?-e e) env)])
       (cond
         [(..? v)  ; zaporedje
          (let ([prvi (fri (..-e1 v) env)]
                [drugi (fri (..-e2 v) env)])
            (if (false? prvi)
                (false) ; zaporedje vsebuje (false), zato naj bo rezultat (false)
                (cond
                  [(..? drugi) (fri (all? drugi) env)] ; če je drugi element zaporedje -> rekurziven klic
                  [(false? drugi) (false)]
                  [#t (true)])))]
         [(s? v)  ; množica
          (if (subset? (set (false)) (s-e v)) ; če je (false) podmnožica, potem je (false) vsebovan
              (false)  ; (false) je v množici
              (true))]))]

    ; Če zaporedje (ali množica) e1 vsebuje vsaj kašno vrednost, ki ni (false), potem je rezultat izraza (any? e1) enak (true), drugače (false)
    [(any?? e)
     (let ([v (fri (any?-e e) env)])
       (cond
         [(..? v)  ; zaporedje
          (let ([prvi (fri (..-e1 v) env)]
                [drugi (fri (..-e2 v) env)])
            (if (not (false? prvi))
                (true) ; zaporedje vsebuje (false), zato naj bo rezultat (false)
                (cond
                  [(..? drugi) (fri (any? drugi) env)] ; če je drugi element zaporedje -> rekurziven klic
                  [(false? drugi) (false)]
                  [#t (true)])))]
         [(s? v)  ; množica
          (if (or
               (proper-subset? (set (false)) (s-e v))
               (not (subset? (set (false)) (s-e v)))) ; če (false) ni podmnožica ali če je (false) proper-subset, potem (s-e v) vsebuje vsaj eno vrednost, ki ni (false)
              (true)
              (false))]))]

    ; =================================
    ; SPREMENLJIVKE
    ; =================================
    
    [(vars? e)
     (let ([s (vars-s e)]
           [v (vars-e1 e)]
           [izraz (vars-e2 e)])
       (fri izraz (dodajVOkolje env s v)))]

    [(valof? e)
     (let ([ime (valof-s e)])
       (cdr (assoc ime (reverse env))))]


    ; =================================
    ; FUNKCIJE IN PROCEDURE
    ; =================================
    [(fun? e)
     (closure env e)]
    [(proc? e)
     e]
    [(closure? e)
     e]
    [(call? e)
     (let ([izraz (fri (call-e e) env)]
           [argumentiKlica (let ([tmp (call-args e)]) ; vrednosti argumentov
                             (cond
                               [(null? tmp) tmp]
                               [(list? tmp) (map (lambda (x) (fri x env)) tmp)] ; evalviraj vsak argument v seznamu
                               [#t (fri tmp env)]))]) ; če ni seznam in ni null, je vrednost
       (cond
         [(closure? izraz)
          (let ([telo (fun-body (closure-f izraz))]
                [ime (fun-name (closure-f izraz))]
                [argumentiFunkcije (fun-farg (closure-f izraz))] ; imena argumentov funkcije
                [okolje (closure-env izraz)]) ; leksikografsko okolje funkcije - ob definiciji fun
            (fri telo
                 (dodajVOkolje ; v okolje dodamo še argumente -> V primeru, da je ime funkcije enako imenu argumenta funkcije, argument zasenči funkcijo
                  (if (equal? "" ime) ; preverimo ali gre za anonimno funkcijo
                      okolje ; anonimna funkcija -> je ne dodamo v okolje za rekuriven klic
                      (dodajVOkolje okolje ime izraz)) ; v okolje najprej dodamo ime funkcije, ki je povezano z ovojnico, da omogočimo rekurzivne klice
                  argumentiFunkcije argumentiKlica)))]

         [(proc? izraz)
          (let ([telo (proc-body izraz)]
                [ime (proc-name izraz)])
            (fri telo
                 (if (equal? "" ime) ; preverimo ali gre za anonimno procedure
                      env ; anonimna funkcija -> je ne dodamo v okolje za rekuriven klic
                      (dodajVOkolje env ime izraz))))] ; v okolje najprej dodamo ime procedure, ki je povezano s proceduro, da omogočimo rekurzivne klice
         
         [#t (error "sintaksa izraza ni pravilna")]))]
    
    
    [#t (error "sintaksa izraza ni pravilna")]
    ))


(require racket/trace)
;(trace fri)
