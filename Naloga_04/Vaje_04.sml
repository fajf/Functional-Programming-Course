(* dolgi zapis
    val <ime fun.> = fn x => fn y ... => <telo fun.>
dolgi zapis (rekurzivne funkcije)
    val rec <ime fun.> = fn x => fn y ... => <telo fun.>
kratek zapis
    fun <ime fun.> x y ... = <telo fun.> *)


val f : int -> (int -> int) =
    fn x => (fn y => x * y);

fun f x: int -> int = fn y => x * y;
fun f x y = x*y;

(* (f↦(x↦(y↦f(x,y)))) *)
val curry = fn f => fn x => fn y => f(x, y);
fun curry f = fn x => fn y => f(x, y);
fun curry f x y = f(x, y);

val uncurry = fn f => fn (x, y) => f x y;
fun uncurry f = fn (x, y) => f x y;
fun uncurry f(x, y) = f x y;

fun swap f x y = f y x;

val compose = fn (f, g) => fn x => f(g x);
fun compose (f, g) = fn x => f(g x);
fun compose (f, g) = f o g;
val compose = op o;

fun compose2 f g x = f (g x);
fun compose2 f g = f o g;

fun apply (f, x) = f x;
infixr 2 $;
fun f $ x = f x;
fun compose f g x = f $ g $ x;
fun compose f g x = f $ g x;

fun apply2 f x = f x;

fun foldl f acc (x :: xs) = foldl f (f(x, acc)) xs
|   foldl _ acc [] = acc;
val foldl = List.foldl;

fun foldl2 f acc (x :: xs) = foldl2 f (f x acc) xs
|   foldl2 _ acc [] = acc;

fun foldl2 f acc xs = foldl (uncurry f) acc xs;
fun foldl2 f acc = foldl (uncurry f) acc;
fun foldl2 f = foldl (uncurry f);
fun foldl2 f = foldl $ uncurry f;

fun rev xs = foldl2 (fn x => fn acc => x :: acc) [] xs;
val rev = List.rev;

fun foldr f acc xs = foldl2 f acc (rev xs);
fun foldr f acc = foldl2 f acc o rev;

fun foldr2 f acc xs = foldl f acc (rev xs);
fun foldr2 f = foldr $ curry f;

fun map f xs = List.foldr (fn (x, acc) => f x :: acc) [] xs;
val map = List.map;

(* fun filter *)

fun iterate 0 f x = x
|   iterate n f x = f (iterate (n-1) f x);

fun D f = fn x =>
    let val h: real = 1E~5 in (f(x + h) - f(x - h)) / (2.0 * h) end;

(* (D (fn x => 2.0 * x + 1.0)) 3.0;
D Math.sin Math.pi; *)

fun Dn n f = iterate n D f;

Dn 0 Math.sin (Math.pi/6.0);
Dn 1 Math.sin (Math.pi/6.0); (* cos(pi) *)
Dn 2 Math.sin (Math.pi/6.0); (* -sin(pi) *)
Dn 3 Math.sin (Math.pi/6.0);

(* fun partial *)

(* fun gradient *)

(* grad(x^2 - y^2) = (2 x, -2 y) *)
(* gradient f #[0.0, 0.0];
gradient f #[0.0, 1.0];
gradient f #[1.0, 0.0];
gradient f #[4.0, 3.0]; *)

(* fun laplacian *)

(* Δ(x^2 - y^2) = 0 *)
(* laplacian f #[0.0, 0.0];
laplacian f #[0.0, 1.0];
laplacian f #[1.0, 0.0];
laplacian f #[4.0, 3.0]; *)
