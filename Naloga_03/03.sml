val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

datatype natural = Succ of natural | One;
exception NotNaturalNumber;

fun zip (x, y) =
    case (x,y) of 
        (x::xs, y::ys) => (x,y) :: zip(xs,ys)
        | _ => [];


fun unzip (s : ('a * 'b) list) : 'a list * 'b list =
let
    fun unzip (f, (b,c) :: xs) =
        case xs of
            nil => f (b,c) :: nil
            | xs => f (b,c) :: unzip(f, xs)
in
    case s of
        [] => ([],[])
        | _ => (unzip( (fn (x,y) => x), s), unzip( (fn (x,y) => y), s))
end;

(*)
fun unzip' (s : ('a * 'b) list) : 'a list * 'b list =
    case s of
        [] => ([],[])
        | (x,y) :: xs => (x :: #1 unzip'(xs), y :: #2 unzip'(xs));*)

fun subtract (a, b) =
         case (a,b) of
            (One, _) => raise NotNaturalNumber
            | (Succ x, One) => x
            | (Succ x, Succ y) => subtract(x,y);

fun any(f, s) =
    case s of
        nil => false
        | s::xs => case (f s) of
                true => true
                | false => any(f, xs);

fun map (f, s) =
    case s of
        nil => nil
        | x::xs => f x :: map(f,xs);

fun filter (f, s) =
    case s of
        nil => nil
        | x::xs => case f x of
                    false => filter(f, xs)
                    | true => x :: filter(f, xs);

fun fold (f, z, s) : 'a =
    case s of
        nil => z
        | x::xs => fold(f, f(z,x), xs);


datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun height lf = 0
    |   height (br (l, _, r)) = Int.max(height l, height r) + 1;

fun imbalance lf = 0
    |   imbalance (br (l, _, r)) = height r - height l;

fun rotate (drevo : 'a bstree, smer : direction) : 'a bstree =
    let
        fun desnaRotacija (br (br (a,b,c), d, e)) = br (a,b, br (c,d,e))
            | desnaRotacija drevo = drevo
        
        fun levaRotacija (br (a, b, br (c,d,e))) = br (br (a,b,c), d, e)
            | levaRotacija drevo = drevo
    in
        case smer of
            R => desnaRotacija(drevo)
            | L => levaRotacija(drevo)
    end;

fun rebalance (drevo : 'a bstree) : 'a bstree =
    case drevo of
        lf => drevo
        | br (a,b,c) => case imbalance (br (a,b,c)) of
                            2 =>( case imbalance c of
                                    1 => rotate(drevo, L)
                                    | ~1 => rotate(br (a, b, rotate(c, R)),L)
                                    | _ => drevo
                            )
                            | ~2 =>( case imbalance a of
                                    1 => rotate(br (rotate(a, L), b, c), R)
                                    | ~1 => rotate(drevo, R)
                                    | _ => drevo
                            )
                            | _ => drevo;

fun avl (f : ('a * 'a -> order), drevo : 'a bstree, e : 'a) : 'a bstree =
    case drevo of
        lf => br (lf, e, lf)
        | br (a,b,c) => case f (e,b) of
                            EQUAL => drevo
                            | GREATER => rebalance (br (a,  b, avl (f, c, e)))
                            | LESS => rebalance (br (avl (f, a, e), b, c));