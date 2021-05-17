(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;




val zip1 = zip([1,2,3], [7,8,9]) = [(1,7),(2,8),(3,9)];
val zip2 = zip([1,2,3], [7,8]) = [(1,7),(2,8)];
val unzip1 = unzip([]) = ([],[]);
val unzip2 = unzip([(1,7),(2,8),(3,9)]) = ([1,2,3],[7,8,9]);
val unzip3 = unzip'([]) = ([],[]);
val unzip4 = unzip'([(1,7),(2,8),(3,9)]) = ([1,2,3],[7,8,9]);
val subtract = subtract(Succ(Succ(One)), Succ(One)) = One;
val any = any((fn x => if x mod 2 = 0 then true else false), [1,3,5,7,9,11]) = false;
val map = map (fn x => x + 10, [1, 2, 3, 4]) = [11,12,13,14];
val filter = filter (fn x => x mod 2 = 0, [1, 2, 3, 4]) = [2,4];
val fold = fold((fn (x,y) => x+y), 10, [1,2,3,4]) = 20;




(* izpis drevesa po nivojih *)
fun showTree (toString : 'a -> string, t : 'a bstree) =
let fun strign_of_avltree_level (lvl, t) = case t of  
        lf => if lvl = 0 then "nil" else "   "
    |   br (l, n, r) =>
        let val make_space = String.map (fn _ => #" ")
            val sn = toString n
            val sl = strign_of_avltree_level (lvl, l)
            val sr = strign_of_avltree_level (lvl, r)
        in if height t = lvl
            then make_space sl ^ sn ^ make_space sr
            else sl ^ make_space sn ^ sr
        end
    fun print_levels lvl =
        if lvl >= 0
        then (print (Int.toString lvl ^ ": " ^ strign_of_avltree_level (lvl, t) ^ "\n");
                    print_levels (lvl - 1))
        else ()
  in  print_levels (height t)
end;

(* primeri vstavljanja elementov v AVL drevo *)
fun avlInt (t, i) = avl (Int.compare, t, i);
fun showTreeInt t = showTree(Int.toString, t);

val tr = lf : int bstree;
val _ = showTreeInt tr;
val tr = avlInt (tr, 1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 2);
val _ = showTreeInt tr;
val tr = avlInt (tr, 3);
val _ = showTreeInt tr;
val tr = avlInt (tr, 4);
val _ = showTreeInt tr;
val tr = avlInt (tr, 5);
val _ = showTreeInt tr;
val tr = avlInt (tr, 6);
val _ = showTreeInt tr;
val tr = avlInt (tr, 7);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~4);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~3);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~2);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 0);
val _ = showTreeInt tr;

val from0to13 = fold (fn (z, x) => avl (Int.compare, z, x), lf, List.tabulate (14, fn i => i));