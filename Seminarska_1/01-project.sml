datatype 'a expr = !! of 'a expr
| \/ of 'a expr * 'a expr
| /\ of 'a expr * 'a expr
| <=> of 'a expr * 'a expr
| ==> of 'a expr * 'a expr
| V of 'a
| T | F;
infix 5 <=>;
infixr 6 ==>;
infix 7 \/;
infix 8 /\;

datatype 'a expression = Not of 'a expression
| Or of 'a expression list
| And of 'a expression list
| Eq of 'a expression list
| Imp of 'a expression * 'a expression
| Var of 'a
| True | False;

datatype 'a stream = Next of 'a * (unit -> 'a stream);
fun lcg seed =
        let fun lcg seed =
                    Next (seed, fn () =>
                            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
        in lcg (LargeInt.fromInt seed) end;

fun int2bool i = LargeInt.mod (i, 2) = 1;

exception InvalidCNF;
exception NotImplemented;

Control.Print.printDepth := 100;
Control.Print.printLength := 1000;
Control.Print.stringDepth := 1000;



(*=================================================================*)

fun getVars (exp : ''a expression) : ''a list =
        let
            val lst = case exp of
                Not e1 => getVars e1
            |   Or e => foldl (fn (x,acc) => acc @ (getVars x)) [] e
            |   And e => foldl (fn (x,acc) => acc @ (getVars x)) [] e
            |   Eq e => foldl (fn (x,acc) => acc @ (getVars x)) [] e
            |   Imp (e1,e2) => getVars e1 @ getVars e2
            |   Var v => [v]
            |   True => []
            |   False => []
            fun odstraniDuplikate lst =
                    case lst of
                        [] => []
                    | z::zs => z :: odstraniDuplikate (List.filter 
                            (fn x => x <> z)
                            zs)
        in
            odstraniDuplikate lst
        end;



fun eval (vars : ''a list) (exp : ''a expression) : bool =
        let
            fun equality lst =
                    let
                        val evaluated = foldl (fn (x,acc) => acc @ [(eval vars x)]) [] lst
                    in
                        if 
                            List.length evaluated = 
                            List.length 
                            (List.filter 
                                (fn y => y = (hd evaluated))
                                evaluated)
                        then true else false
                    end
            fun implication (a,b) =
                    case (a,b) of
                        (true,false) => false
                    | _ => true       
        in
            case exp of
                Not e1 => not (eval vars e1)
            | Or e => foldl (fn (x,acc) => acc orelse (eval vars x)) false e
            | And e => foldl (fn (x,acc) => acc andalso (eval vars x)) true e
            | Eq e => equality e
            | Imp (e1,e2) => implication ( (eval vars e1),(eval vars e2) )
            | Var v => if (List.length (List.filter (fn x => x = v) vars)) > 0 then true else false
            | True => true
            | False => false
        end;


fun rmEmpty (exp : 'a expression) : 'a expression =
        let
            fun rmOr lst =
                    case lst of
                        [] => False
                    | x :: [] => rmEmpty x
                    | _ => Or (foldl (fn (x,acc) => acc @ [(rmEmpty x)]) [] lst)
            fun rmAnd lst =
                    case lst of
                        [] => True
                    | x :: [] => rmEmpty x
                    | _ => And (foldl (fn (x,acc) => acc @ [(rmEmpty x)]) [] lst)
            fun rmEq lst =
                    case lst of
                        [] => True
                    | x :: [] => True
                    | _ => Eq (foldl (fn (x,acc) => acc @ [(rmEmpty x)]) [] lst)
        in
            case exp of
                Not e => Not (rmEmpty e)
            | Or e => rmOr e
            | And e => rmAnd e
            | Eq e => rmEq e
            | Imp (e1,e2) => Imp ( (rmEmpty e1),(rmEmpty e2) )
            | Var v => Var v
            | True => True
            | False => False
        end;




fun beautify (exp : 'a expression) : 'a expr =
        let
            val emp = rmEmpty exp
        
            fun beaOr lst = 
                    case lst of
                        x :: [] => (beautify x)
                    | x :: xs => (beaOr xs) \/ (beautify x)

            fun beaAnd lst = 
                    case lst of
                        x :: [] => (beautify x)
                    | x :: xs => (beaAnd xs) /\ (beautify x)

            fun beaEq (x :: xs) = 
                    case (tl xs) of
                        [] => (beautify (hd xs)) <=> (beautify x)
                    | _ => (beaEq xs) /\ ((beautify (hd xs)) <=> (beautify x))

        in
            case emp of
                Not e => !! (beautify e)
            | Or e => beaOr (List.rev e)
            | And e => beaAnd (List.rev e)
            | Eq e => beaEq (List.rev e)
            | Imp (e1,e2) => (beautify e1) ==> (beautify e2)
            | Var v => V v
            | True => T
            | False => F
        end;

fun pushNegations (exp : 'a expression) : 'a expression =
        let
            fun pushNegList lst =
                    foldl (fn (x,acc) => acc @ [(pushNegations (Not x))] ) [] lst
            fun pushList lst =
                    foldl (fn (x,acc) => acc @ [(pushNegations x)] ) [] lst
        in
            case (rmEmpty exp) of
                Not e => 
                    (case e of
                            Not f => (pushNegations f)
                        | Or f => And (pushNegList f)
                        | And f => Or (pushNegList f)
                        | Eq f => And [Or (pushNegList f), Or (pushList f)]
                        | Imp (e1,e2) => And [pushNegations (e1), (pushNegations (Not (e2)))]
                        | _ => Not e)
            | Or e => Or (pushList e)
            | And e => And (pushList e)
            | Eq e => Eq (pushList e)
            | Imp (e1,e2) => Imp (pushNegations (e1), pushNegations (e2))
            | Var v => Var v
            | True => True
            | False => False
        end;

fun rmConstants (exp : ''a expression) : ''a expression =
        let
            fun filterTrue lst = List.filter (fn x => if (rmConstants x) = True then false else true) lst
            fun filterFalse lst = List.filter (fn x => if (rmConstants x) = False then false else true) lst
        
            fun rmConsNot e =
                    case (rmConstants e) of
                        True => False
                    |   False => True
                    |   Not a => rmConstants a
                    |   _ => Not (rmConstants e)

            fun rmConsAnd lst =
                    if List.length (lst) = List.length (filterFalse lst) 
                    then rmEmpty (And (List.map (fn x => rmConstants x) (filterTrue (lst))))
                    else False

            fun rmConsOr lst =
                    if List.length (lst) = List.length (filterTrue lst)
                    then rmEmpty (Or (List.map (fn x => rmConstants x) (filterFalse lst)))
                    else True

            fun rmConsImpl (e1,e2) =
                    case (rmEmpty (rmConstants e1), rmEmpty (rmConstants e2)) of
                        (a,False) => rmConstants (Not a)
                    |   (a,True) => True
                    |   (True,b) => rmConstants (b)
                    |   (False,b) => True
                    |   (a,b) => Imp (a,b)

            fun rmConsEq lst =
                    (*ce je v seznamu True in tudi False je to avtomatsko False. Ker True <=> False -> False*)
                    if 
                        List.length lst <> List.length (filterTrue lst)
                        andalso
                        List.length (filterTrue lst) <> List.length (filterTrue (filterFalse lst))
                    then False
                    else    (* v seznamu ni obeh konstant True in False *)
                        if List.length lst <> List.length (filterTrue lst)    (* ali je v seznamu True *)
                        then rmConsAnd lst    (* v seznamu je True *)
                        else   (* v seznamu ni True*)
                            if List.length lst <> List.length (filterFalse lst) (* ali je v seznamu False *)
                            then rmConsAnd (List.map (fn x => Not x) (filterFalse lst)) (* v seznamu je False *)
                            else rmEmpty (Eq (List.map (fn x => rmConstants x) lst))

        in
            case (rmEmpty exp) of
                Not e => rmConsNot e
                | Or e => rmConsOr e
                | And e => rmConsAnd e
                | Eq e => rmConsEq e
                | Imp e => rmConsImpl e
                | Var v => Var v
                | True => True
                | False => False
        end;

fun rmVars (express : ''a expression) : ''a expression =
        let
            val exp = rmEmpty express

            fun rmVarsImp (e1,e2) =
                    if (rmVars e1) = (rmVars e2) then True else Imp ((rmVars e1),(rmVars e2))

            fun rmVarsAnd lst =
                    case lst of
                        [] => []
                    | _ => (rmVars (hd lst)) :: rmVarsAnd (List.filter (fn x => (if (rmVars (hd lst)) = (rmVars x) then false else true) ) (tl lst))
        in
            case (exp) of
                Not e => Not (rmVars e)
            | Or e => rmEmpty (Or (rmVarsAnd e))    (* Or in And delujeta povsem enako *)
            | And e => rmEmpty (And (rmVarsAnd e))
            | Eq e => rmEmpty (Eq (rmVarsAnd e))
            | Imp e => rmVarsImp e
            | Var v => Var v
            | True => True
            | False => False
        end;

fun simplify (exp : ''a expression) : ''a expression =
    let
        val simpl = (rmVars (pushNegations (rmConstants exp)));
    in
        if exp = simpl
        then exp
        else simplify simpl
    end;

fun prTestEq (seed : int) (exp1 : ''a expression) (exp2 : ''a expression) =
    let
        val vars1 = getVars exp1
        val vars2 = getVars exp2
        val vars2uniq = 
            List.filter 
                (fn x => if List.exists (fn y => x = y) vars1 then false else true)
                vars2
        val vars = vars1 @ vars2uniq

        fun prTest (Next(a,f)) lst =
            case lst of
                [] => []
            |   (x::xs) =>
                    (case (int2bool a) of
                        true => x :: prTest (f()) xs
                    |   false => prTest (f()) xs)

        val randomVars = prTest (lcg seed) vars
    in
        (eval randomVars exp1) = (eval randomVars exp2)
    end;

fun toWolframLang (f : ('a -> string)) (exp : 'a expression) : string =
    let
        fun toWolframList fu lst = 
            String.concatWith (", ") (foldl (fn (x,acc) => acc @ [(toWolframLang f x)] ) [] lst)
    in
        case exp of
        Not e => "Not[" ^ (toWolframLang f e) ^ "]"
    |   Or e => "Or[" ^ (toWolframList f e) ^ "]"
    |   And e => "And[" ^ (toWolframList f e) ^ "]"
    |   Eq e => "Equivalent[" ^ (toWolframList f e) ^ "]"
    |   Imp (e1,e2) => "Implies[" ^ (toWolframLang f e1) ^ ", " ^ (toWolframLang f e2) ^ "]"
    |   Var v => "Var[\"" ^ (f v) ^ "\"]"
    |   True => "True"
    |   False => "False"
    end;

fun satSolver (exp : ''a expression) =
    let
        (*Not lahko stoji samo poleg spremenljivke*)
        fun checkKNOnot exp =
            case exp of
                Var v => true
            |   Not e =>
                    (case e of
                        Var v => true
                    |   True => true
                    |   False => true
                    |   Not v => checkKNOnot (v)
                    |   _ => false)
            |   True => true
            |   False => true
            |   _ => false

        (*v izrazu Or je lahko le Var ali pa Not Var*)
        fun checkKNOor lst =
            case lst of 
                [] => true
                | _ => List.all (fn x => (checkKNOnot x)) lst

        fun checkKNOand lst =
            case lst of
                [] => [true]
            |   (x::xs) => 
                    (case x of 
                        Or y => checkKNOor y
                    |   True => true
                    |   False => true
                    |   Var v => true
                    |   Not e => checkKNOnot e
                    |   _ => false) :: (checkKNOand xs)

        fun checkKNO exp =
            case exp of
                Var v => true
            |   True => true
            |   False => true
            |   Not v => checkKNOnot v
            |   Or lst => checkKNOor lst
            |   And lst => List.all (fn x => x) (checkKNOand lst)
            |   _ => false

        val isKNO = (checkKNO exp)

        fun poenostavi ((var : ''a), (boo : ''a expression)) (exp : ''a expression) : ''a expression =
            case exp of
                Var v => 
                    if v = var then boo else (Var v)
            |   Not e => 
                    (case e of
                        Var v => if v = var then (Not boo) else (Not (Var v))
                    |   True => Not True
                    |   False => Not False) 
            |   True => True
            |   False => False
            |   Or lst => Or (List.map (fn x => (poenostavi (var,boo) x) ) lst)
            |   And lst => And (List.map (fn x => (poenostavi (var,boo) x) ) lst)
            

        fun korakEna (preglej : ''a expression) (exp : ''a expression) (seznamOdstranjenih : ''a list) =
            case preglej of
                True => (exp, seznamOdstranjenih)
            |   False => (exp, seznamOdstranjenih)
            |   Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
            |   Not e =>
                    (case e of
                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih)
                    |   True => (exp, seznamOdstranjenih)
                    |   False => (exp, seznamOdstranjenih)
                    )
            |   Or lst =>
                    (case lst of
                        [] => (exp, seznamOdstranjenih)
                    |   (x :: nil) => 
                            (case x of
                                Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
                            |   True => (exp, seznamOdstranjenih)
                            |   False => (exp, seznamOdstranjenih)
                            |   Not e =>
                                    (case e of
                                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih)
                                    |   True => (exp, seznamOdstranjenih)
                                    |   False => (exp, seznamOdstranjenih)
                                    )
                            )
                    |   _ => (exp, seznamOdstranjenih))
            |   And lst => foldl (fn (x,acc) => (korakEna x (#1(acc)) (#2(acc))) ) (exp,seznamOdstranjenih) lst
    
        val konecKorakaEna = if isKNO then (korakEna (exp) (exp) []) else (False, [])

        fun izberiVrednostSpremenljivke exp seznamOdstranjenih =
            case exp of
                Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
            |   Not e =>
                    (case e of
                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih))
            |   Or lst =>
                    (case lst of
                        (x :: xs) => 
                            (case x of
                                Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
                            |   Not e =>
                                    (case e of
                                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih)))
                    )
            |   And lst =>
                    (case lst of
                        (x :: xs) => 
                            (case x of
                                Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
                            |   Not e =>
                                    (case e of
                                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih))
                            |   Or lst1 =>
                                    (case lst1 of
                                        (y :: ys) => 
                                            (case y of
                                                Var v => ((poenostavi (v,True) exp), (seznamOdstranjenih @ [v]))
                                            |   Not e =>
                                                    (case e of
                                                        Var v => ((poenostavi (v,False) exp), seznamOdstranjenih)
                                                    )
                                            )
                                    )
                            )
                    )

        fun izberiVrednostSpremenljivkeNeg exp seznamOdstranjenih =
            case exp of
                Var v => ((poenostavi (v,False) exp), (seznamOdstranjenih))
            |   Not e =>
                    (case e of
                        Var v => ((poenostavi (v,True) exp), seznamOdstranjenih @ [v]))
            |   Or lst =>
                    (case lst of
                        (x :: xs) => 
                            (case x of
                                Var v => ((poenostavi (v,False) exp), (seznamOdstranjenih))
                            |   Not e =>
                                    (case e of
                                        Var v => ((poenostavi (v,True) exp), seznamOdstranjenih @ [v])))
                    )
            |   And lst =>
                    (case lst of
                        (x :: xs) => 
                            (case x of
                                Var v => ((poenostavi (v,False) exp), (seznamOdstranjenih))
                            |   Not e =>
                                    (case e of
                                        Var v => ((poenostavi (v,True) exp), seznamOdstranjenih @ [v]))
                            |   Or lst1 =>
                                    (case lst1 of
                                        (y :: ys) => 
                                            (case y of
                                                Var v => ((poenostavi (v,False) exp), (seznamOdstranjenih))
                                            |   Not e =>
                                                    (case e of
                                                        Var v => ((poenostavi (v,True) exp), seznamOdstranjenih @ [v])
                                                    )
                                            )
                                    )
                            )
                    )

        fun korakTri exp seznamOdstranjenih =
                case (satSolver (#1(izberiVrednostSpremenljivke exp seznamOdstranjenih))) of
                    SOME lst => 
                        SOME (
                            (#2(izberiVrednostSpremenljivke exp seznamOdstranjenih))
                            @ lst
                        )
                |   NONE =>
                        (case (satSolver (#1(izberiVrednostSpremenljivkeNeg exp seznamOdstranjenih))) of
                            SOME lst => 
                                SOME (
                                    (#2(izberiVrednostSpremenljivkeNeg exp seznamOdstranjenih))
                                    @ lst
                                )
                        |   NONE => NONE
                        )
                
                (*(izberiVrednostSpremenljivkeNeg exp seznamOdstranjenih)*)

                (*
                (
                #1(satSolver
                    (#1(izberiVrednostSpremenljivke exp seznamOdstranjenih))),
                
                #2(izberiVrednostSpremenljivke exp seznamOdstranjenih) 
                @ 
                #2(satSolver
                    (#1(izberiVrednostSpremenljivke exp seznamOdstranjenih)))    
                )*)
                
            
        and satSolver' exp seznamOdstranjenih =
            if rmConstants(#1(konecKorakaEna)) = True
            then SOME (#2(konecKorakaEna)) (*(rmConstants(#1(konecKorakaEna)), (#2(konecKorakaEna)))*)   (* SOME (#2(konecKorakaEna)) *)
            else 
                (if rmConstants(#1(konecKorakaEna)) = False
                then NONE (*(rmConstants(#1(konecKorakaEna)), (#2(konecKorakaEna)))*) (*NONE*)
                else (korakTri (rmConstants(#1(konecKorakaEna))) (#2(konecKorakaEna))))  (* KORAK TRI*)
    in
        if (not isKNO)
        then raise InvalidCNF
        else satSolver' exp []
    end;

fun bruteforce exp = raise NotImplemented
fun problemReduction a b c = raise NotImplemented
fun solutionRepresentation lst = raise NotImplemented