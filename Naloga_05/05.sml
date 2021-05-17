val _ = Control.polyEqWarn := false;

structure Rational =
struct
    type rational = int * int
    
    exception BadRational

    fun gcd (a,b) =
        case (a,b) of
            (a,0) => a
        |   (a,b) => gcd (b, a mod b)

    fun okr (a,b) =
        let
			val g = gcd(a, b)
			val aa = a div g
			val bb = b div g
		in
            if b = 0
                then raise BadRational
                else
                    if b > 0 
                        then (aa, bb)
                        else (~aa, ~bb)
		end

    fun makeRational (a,b) = okr (a,b)   

    fun neg (a,b) = okr (~a,b)

    fun inv (a,b) = okr (b,a)

    fun add ((a,b), (c,d)) = okr (a*d+b*c, b*d)

    fun mul ((a,b), (c,d)) = okr (a*c, b*d)

    fun toString (a,b) =
        case (a,b) of
            (a,1) => Int.toString a
            | (a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)
end;



signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

funsig SETFN (Eq : EQ) = SET

functor SetFn (a : EQ) : SET = 
struct
    type item = a.t

    type set = item list

    val empty : set = []

    fun singleton (x : item) : set = [x]

    fun obstaja (lst : set) y =
        case lst of
            [] => false
        |   (x::xs) =>
            if a.eq x y
                then true
                else obstaja xs y

    fun union (lst1 : set) (lst2 : set) : set =
        case lst2 of
            [] => lst1
        |   x::xs => 
            if (obstaja lst1 x)
                then union lst1 xs
                else union (lst1 @ [x]) xs

    fun difference (lst1 : set) (lst2 : set) : set =
        let
            fun difference (lst : set) (acc : set) : set =
                case (lst, lst2) of
                    (_, []) => lst
                |   ([],_) => acc
                |   ((x::xs), _) =>
                        if (obstaja lst2 x)
                            then difference xs acc
                            else (difference xs (acc @ [x]))
        in
            difference lst1 []
        end

    fun subset (lst1 : set) (lst2 : set) : bool =
        case lst1 of
            [] => true
        |   (x::xs) =>
            if (obstaja lst2 x)
                then subset xs lst2
                else false
end;