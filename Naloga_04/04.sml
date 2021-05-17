(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)
fun reduce (f : ('a -> 'b -> 'a)) (z : 'a) (lst : 'b list) : 'a =
    case lst of 
        [] => z
        | x::xs => reduce f (f z x) xs;

reduce (fn z => fn x => z+x) 0 [1,2,3];

(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. *)
val squares : int list -> int list =
    fn lst => List.map (fn x => x*x) lst;

squares [1,2,3,4,5];

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. *)
val onlyEven : int list -> int list =
    fn lst => List.filter (fn x => if x mod 2 = 0 then true else false) lst;

onlyEven [0,1,2,3,4,5,6];

(* Vrne najboljši niz glede na funkcijo f (prvi arg.). Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f (first arg.). The function f compares two strings and returns true if the first string is better than the other. Use List.foldl. The best string in an empty list is an empty string. *)
fun bestString (f : (string * string -> bool)) (lst : string list) : string =
    List.foldl (fn (a,b) => if f (a,b) then a else b) "" lst;

bestString (fn (a,b) =>  case String.compare(a,b) of
                                GREATER => true
                                | _ => false ) ["a", "b", "c"];

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
fun largestString (lst : string list) : string =
    bestString 
        (fn (a,b) =>  case String.compare(a,b) of
                                GREATER => true
                                | _ => false )
        lst;

largestString ["a", "b", "c", "d"];

(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
fun longestString (lst : string list) : string =
    bestString
        (fn (a,b) => if size a > size b then true else false)
    lst;

longestString ["", "a", "ab", "abc", "d"];

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)
(* Sorts the list with quicksort. First argument is a compare function. *)
fun quicksort (f : ('a * 'a -> order)) (lst : 'a list) : 'a list =
    case lst of
        [] => []
        | x::xs => 
            quicksort f (List.filter 
                (fn a => case f (a,x) of
                    LESS => true
                    | _ => false) 
                xs)
            @
            [x]
            @
            quicksort f (List.filter 
                (fn a => case f (a,x) of
                    GREATER => true
                    | _ => false) 
                xs);

quicksort (fn (a,b) => if a > b then GREATER else LESS) [5,4,3,2,1,9,10,6,7,8];




(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors. Use List.foldl and ListPair.map. *)
fun dot (a : int list) (b : int list) : int =
    List.foldl (fn (x,acc) => x + acc) 0 (ListPair.map (fn (x,y) => x*y) (a,b));

dot [1,2,3] [1,2,3];
dot [1,2,3] [];


(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
(* Returns the transpose of m. The matrix m is given with row vectors from top to bottom:
  [[1,2,3],[4,5,6],[7,8,9]] represents the matrix
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)
fun transpose (lst : 'a list list) : 'a list list =
    case lst of
        [] => []
        | [] :: preostanek => transpose preostanek
        | (x::xs) :: preostanek => 
            (x :: map hd preostanek) :: transpose (xs :: map tl preostanek);

transpose [[1,2,3],[4,5,6],[7,8,9]];
transpose [[3,1,2],[~2,0,5]];


(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
fun multiply (a : int list list) (b : int list list) : int list list =
    case a of
        [] => []
        | x::xs => (map (fn y => dot x y) (transpose b)) :: (multiply xs b);


multiply [[~1,3],[0,5],[2,5]] [[3,1,2],[~2,0,5]]; 


(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count). The unix tool uniq -c works similarly. *)
(*: (''a * int) list*)
fun group (lst : ''a list) =    
    case lst of
        [] => []
        | x :: [] => [(x,1)]
        | x :: xs => 
            if x = hd xs then 
                (x, (#2 (hd (group xs)))+1) :: (tl (group xs))
            else
                (x,1) :: group xs;

group [1,1,1,1,2,2,3,1,2,3];
group ["a","a","b","a","c","c","a"];

(*)
(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes. The order of elements inside each equivalence class should be the same as in the original list. The equivalence relation is given with a function f, which returns true, if two elements are equivalent. *)
val equivalenceClasses = fn : ('a -> 'a -> bool) -> 'a list -> 'a list list
*)