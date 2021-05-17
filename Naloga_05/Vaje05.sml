(* val n = ref 3;

!n;
n := !n * 2;
!n;


val n = ref 10;
val i = ref 0;

while (!n > 0) do
    (i := !i + !n; n := !n-1);
(* n = 0, i = 55 *)

print "konec while\n";
!n;
!i;


val n = ref 10;
val i = ref 0;

let fun myWhile () =
    if (!n > 0)
    then (i := !i + !n; n := !n-1; myWhile ())
    else ()
in myWhile () end;
(* n = 0, i = 55 *)

print "konec myWhile\n";
!n;
!i;


val sez = Array.fromList [6, ~1, 99, 10, ~14, 5, 33];

fun swap (a, i, j) =
    let
        val prvi = Array.sub(a, i)
        val drugi = Array.sub(a, j)
    in
        Array.update(a, i, drugi); Array.update(a, j, prvi)
    end;


fun bubbleSort a =
    let
        val n = ref (Array.length a)
        val i = ref 0
        val swapped = ref true
    in
        while (!swapped) do (
            i := 1;
            swapped := false;
            while (!i < !n) do (
                (if Array.sub(a, !i -1) > Array.sub(a, !i)
                then (swap (a, !i - 1, !i); swapped := true)
                else ()); i := !i + 1)
            )
    end; *)


  

(* 
signature TEST =
sig
    val test : int -> int
end;

signature TEST2 =
sig
    val test : int -> int
    val test2 : int -> int
end;

structure Test2A =
struct
    fun test x = x + 3
    fun test2 x = x + 5
end;

structure Test2B =
struct
    fun test x = x + 30000
    fun test2 x = x + 50000
end;



signature MATH =
sig
    type t

    structure X : TEST

    val pi : real
    val black : int -> t
end;

structure Math : MATH =
struct
    type t = int

    structure X = Test2A

    val e = 2.7181
    val pi = 3.14159265
    fun black box = box * box * box
end;

3 : Math.t;

structure Math : MATH =
struct
    open Math
    local open List
    in
        val fold = foldl
    end
end;

(* 3 : Math.t; *)  *)



signature COMPLEX =
sig
    type complex

    val complex : Real.real -> Real.real -> complex
    val i : complex
    val re : complex -> Real.real
    val im : complex -> Real.real

    val neg : complex -> complex
    val inv : complex -> complex

    val * : complex * complex -> complex
    val + : complex * complex -> complex

    val toString: complex -> String.string
end;

structure Complex :> COMPLEX =
struct
    type complex = Real.real * Real.real

    fun complex a b = (a, b)

    val i : complex = (0.0, 0.1)
    fun re ((a, b) : complex) = a
    fun im ((a, b) : complex) = b

    fun neg ((a, b) : complex) = let open Real in (~ a, ~ b) end
    fun inv ((a, b) : complex) = let open Real val s = a * a + b * b in (a / s, ~ b / s) end

    fun conj ((a, b) : complex) = (a, Real.~ b)

    fun op* ((a1, b1) : complex, (a2, b2) : complex) = let open Real in (a1 * a2 - b1 * b2, a1 * b2 + a2 * b1) end
    fun op+ ((a1, b1) : complex, (a2, b2) : complex) = let open Real in (a1 + b1, a2 + b2) end

    fun toString ((a, b) : complex) = Real.toString a ^ " + " ^ Real.toString b ^ "i"
end; 



signature KEY =
sig
    type key
    val sameKey : key -> key -> bool
end;

signature SET =
sig
    structure Key : KEY
    type item
    type set
    val mkEmpty : unit -> set
    val toList : set -> item list
    val add : set -> item -> unit
    val subtract : set -> item -> unit
    val member : set -> item -> bool
    val isEmpty : set -> bool
    val fold : (item * 'b -> 'b) -> 'b -> set -> 'b
end;


functor ListSetFn (K : KEY) :> SET where type item = K.key =
struct
    structure Key = K
    type item = K.key
    type set = item list ref
    fun mkEmpty () : set = ref []
    fun toList s = !s
    fun member s e = List.exists (K.sameKey e) (!s)
    fun add s e = if member s e then () else (s := e :: (!s))
    fun subtract s e = s := (List.filter (not o K.sameKey e) (!s))
    fun isEmpty s = null (!s)
    fun fold f acc s = List.foldl f acc (!s)
end;
