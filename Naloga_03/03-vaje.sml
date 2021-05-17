val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

datatype ('prvi, 'drugi) seznamParov = 
    Prazen 
    | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;

type 'a multiMnozica = ('a, int) seznamParov;

fun seznamParov (x :: xs, y :: ys) = 
    Element (x, y, seznamParov(xs, ys))
    | seznamParov _ = Prazen;

fun foldl (f: 'b * 'a -> 'b, z: 'b, s: 'a list) =
        case s of
            [] => z
        |   x :: xs => foldl (f, f(z, x), xs);

foldl ((fn (z, x) => x + 1 :: z), [], [1, 2, 3, 4]);

(* foldr' ni repno-rekurzivna *)
fun foldr' (f, z, x :: xs) = 
    f (foldr' (f, z, xs), x)
    | foldr'(_, z, []) = z;

foldr' ((fn (z, x) => x + 1 :: z), [], [1, 2, 3, 4]);

(* sedaj pa je repno-rekurzivna *)
fun foldr (f, z, s) = foldl (f, z, (foldl(fn (z, x) => x :: z, [], s)));

foldr ((fn (z, x) => x + 1 :: z), [], [1, 2, 3, 4]);

(* tole smo napisali na vajah *)
fun map (f, s) = foldr (fn (z, x) => f x :: z, [], s);

map (fn x => x + 1, [1, 2, 3, 4]);

(* tole sem napisal sam *)
fun map' (f, s) =
    case s of
        [] => []
        | x::xs => f x :: map'(f,xs);

map' (fn x => x + 10, [1, 2, 3, 4]);

fun filter (f, s) = foldr (fn (z, x) => if f x then x :: z else z, [], s);

filter (fn x => x mod 2 = 0, [1, 2, 3, 4]);