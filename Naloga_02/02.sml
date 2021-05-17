datatype number = Zero | Succ of number | Pred of number;

fun simp (a : number) : number =
    case a of
        Zero => Zero
        | Succ b => (case simp b of
                        Pred c => c
                        | c => Succ c)
        | Pred b => (case simp b of
                        Succ c => c
                        | c => Pred c);

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
    case simp a of
        Zero => Zero
        | Succ b => Pred (neg(b))
        | Pred b => Succ (neg(b));


(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
    case simp a of
        Zero => simp b
        | Pred x => add(x, (Pred b))
        | Succ x => add(x, (Succ b));


(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order = 
    case (simp a, simp b) of
        (Zero, Zero) => EQUAL
        | (Zero, Pred _) => GREATER
        | (Zero, Succ _) => LESS
        | (Pred _, Zero) => LESS
        | (Succ _, Zero) => GREATER
        | (Succ _, Pred _) => GREATER
        | (Pred _, Succ _) => LESS
        | (Succ x, Succ y) => comp(x,y)
        | (Pred x, Pred y) => comp(x,y);



(*DREVESA*)
datatype tree = Node of int * tree * tree | Leaf of int;

fun min (t : tree) : int =
    case t of
        Leaf n => n
        | Node (n, t1, t2) => Int.min(n, Int.min(min t1, min t2));

(* Vrne true, če drevo vsebuje element x. *)
fun contains (t : tree, x : int) : bool =
    case t of
        Leaf n => 
            if (n = x) then true else false
        | Node (t, t1, t2) => 
            if (t = x) 
                then true
                else (contains(t1, x) orelse contains(t2, x));


(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
    case tree of
        Leaf n => 1
        | Node (t, t1, t2) => (countLeaves(t1) + countLeaves(t2));


(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
    case tree of
        Leaf _ => 0
        | Node (t, t1, t2) => 2 + countBranches(t1) + countBranches(t2);

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int =
    case tree of
        Leaf _ => 1
        | Node (t, t1, t2) => 1 + Int.max(height(t1), height(t2));

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list =
    case tree of
        Leaf n => [n]
        | Node (t, t1, t2) => toList(t1)@[t]@toList(t2);

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool =
    case tree of
        Leaf _ => true
        | Node (t,t1,t2) => if (height(t1) - height(t2)) < abs 2 
                                then isBalanced(t1) andalso isBalanced(t2)
                                else false;

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) : bool =
    let
        fun isBSTLeft(tree : tree, x : int) =
            case tree of
                Leaf n => if n < x then true else false
                | Node (t,t1,t2) => if t < x andalso isBSTLeft(t1,x) andalso isBSTLeft(t2, x) 
                                        then isBSTLeft(t1,t) andalso isBSTRight(t2,t) 
                                        else false
        
        and isBSTRight(tree : tree, x : int) =
            case tree of
                Leaf n => if n > x then true else false
                | Node (t,t1,t2) => if t > x andalso isBSTRight(t1,x) andalso isBSTRight(t2, x) 
                                        then isBSTLeft(t1,t) andalso isBSTRight(t2,t) 
                                        else false
    in
        case tree of
            Leaf n => true
            | Node (t,t1,t2) => isBSTLeft(t1, t) andalso isBSTRight(t2, t)
    end;