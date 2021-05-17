(* Vrne naslednika števila n. *)
fun next (n : int) : int =
    n + 1;

(* Vrne vsoto števil a in b. *)
fun add (a : int, b : int) : int =
    a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool =
    case (a,b,c) of
        (true,true,_) => true
    |   (true,_,true) => true
    |   (_,true,true) => true
    |   _ => false

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real) *)
fun median (a : real, b : real, c : real) : real =
    Real.min(
        Real.min(
            Real.max(a,b), Real.max(a,c)
        ),
        Real.max(b,c)
    );
    (*
    if (a > b) andalso (a > c) then
        if (b > c) then b else c
    else if (b > c) andalso (b > a) then
        if (a > c) then a else c
    else if (a > b) then a else b
    *)

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika *)
fun triangle (a:int, b:int, c:int) : bool =
    (a + b > c) andalso (a + c > b) andalso (b + c > a);   