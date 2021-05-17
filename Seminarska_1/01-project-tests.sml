use "01-project.sml";

val _ = print "---------- rmConstants ----------\n";
val _ = print "==AND==\n";
rmConstants (And [(Var 1), (Var 2), True]);
rmConstants (And [(Var 1), (Var 2), False]);
rmConstants (And [(Var 1), (Var 2), True, False]);
rmConstants (And [(Var 1), (Var 2), And [True, True]]);
rmConstants (And [True, And [True, True]]);
rmConstants (And [True, And [False, True]]);
val _ = print "==OR==\n";
rmConstants (Or [(Var 1), (Var 2), True]);
rmConstants (Or [(Var 1), (Var 2), False]);
rmConstants (Or [(Var 1), (Var 2), True, False]);
rmConstants (Or [(Var 1), (Var 2), And [True, True]]);
rmConstants (Or [True, Or [True, True]]);
rmConstants (Or [True, Or [False, True]]);
val _ = print "==IMPLIKACIJA==\n";
rmConstants (Imp (False, False)) = True;
rmConstants (Imp (False, True)) = True;
rmConstants (Imp (True, False)) = False;
rmConstants (Imp (True, True)) = True;
rmConstants (Imp (Var 1, And[True,False])) = Not (Var 1);
rmConstants (Imp (Var 1, Or[True,False])) = True;
val _ = print "==EKVIVALENCA==\n";
rmConstants (Eq [False, True]) = False;
rmConstants (Eq [False, Or [True, False, Var 1]]) = False;
rmConstants (Eq [True, And [True, False, Var 1]]) = False;
rmConstants (Eq [True, Or [True, False, Var 1]]) = True;
rmConstants (Eq [False, False]) = True;
rmConstants (Eq [False, True, True]) = False;
rmConstants (Eq [False, True, Var 1]) = False;
rmConstants (Eq [False, Var 1, True]) = False;
rmConstants (Eq [Var 1, Var 2, True]) = And [Var 1, Var 2];
rmConstants (Eq [Var 1, True]) = Var 1;
rmConstants (Eq [Var 1, Var 2, False]) = And [Not (Var 1),Not (Var 2)];
rmConstants (Eq [Var 1, False]) = Not (Var 1);
rmConstants (Eq [Var 1, Var 2]) = Eq [Var 1, Var 2];
rmConstants (Eq [Var 1, Var 2, And [Var 3, True]]) = Eq [Var 1, Var 2, Var 3];
rmConstants (Eq [Var 1, Var 2, Or [Var 3, True]]) = And [Var 1, Var 2];
rmConstants (Eq [Var 1, Var 2, And [Var 3, False]]) = And [Not (Var 1),Not (Var 2)];
val _ = print "==NESTED NOT==\n";
rmConstants (Not (And [Var 4, False, True])) = True;
rmConstants (Not (Eq [Var 4, False, True])) = True;
rmConstants (Not (And [Not (Var 4), Not False])) = Var 4;
rmConstants (Not (Not (Not (Var 4)))) = Not (Var 4);
rmConstants (Eq [Not (Not (Not (Var 4))), Not (Var 4)]);

val _ = print "---------- rmVars ----------\n";
val _ = print "==IMPLIKACIJA==\n";
rmVars ( Not(Imp(Var "a", Var "a")) );
rmVars ( Not(Imp(Var "a", Var "b")) );
rmVars (Imp(And [(Var "a"),(Var "c")], And[(Var "b"),(Var "c")]));
rmVars ( Not(Imp(Var 1, Var 1)) );
rmVars ( Not(Imp(Var 1, Var 2)) );
val _ = print "==AND==\n";
rmVars ( Not(And[(Var "a"), (Var "a")]) );
rmVars ( Not(And[(Var "a"), (Var "b"), (Var "a"), (Var "c")]) );
rmVars ( Not(And[(Var "a"), (Var "b"), (Var "a"), (Var "b"), (Var "c")]) );
val _ = print "==IMPLIKACIJA IN AND==\n";
rmVars ( Not( Imp ( Var "a", And[(Var "a"), (Var "a")])) );
val _ = print "==EKVIVALENCA==\n";
rmVars ( Not(Eq[(Var "a"), (Var "a")]) );
rmVars ( Not(Eq[(Var "a"), (Var "b"), (Var "a"), (Var "c")]) );
rmVars ( Not(Eq[(Var "a"), (Var "b"), (Var "a"), (Var "b"), (Var "c"), Imp(Var "f", Var "f")]) );
val _ = print "==JAVNI TEST 4==\n";
rmVars (Imp (And [Eq [Var 1, Var 0], Eq [Var 1, Var 0], Or [Var 1, Var 1]],And [Eq [Var 1,Var 0],Var 1]));

val _ = print "---------- toWolframLang ----------\n";
val it = toWolframLang (Int.toString) (Var 1);
print (it ^ "\n");
val it = toWolframLang (Int.toString) (Imp (True, (Var 1)));
print (it ^ "\n");
val it = toWolframLang (Int.toString) (Eq [True, (Var 1)]);
print (it ^ "\n");
val it = toWolframLang (Int.toString) (Eq [True, Imp(False, (Var 1))]);
print (it ^ "\n");

val _ = print "---------- satSolver ----------\n";
satSolver (True); (* = SOME []; *)
satSolver (False); (* = NONE; *)
satSolver (Var 1); (* = SOME [1]; *)
satSolver (Not (Var 1)); (* = SOME []; *)
satSolver (Or []);          (* = NONE; *)

val a = satSolver (Or [(Var 1)]);
val b = satSolver (Or [(Var 1), (Var 2)]);

val c = satSolver (Or [False, (Not (Var 2))]);

val d = satSolver (And []);
val e = satSolver (And [Or[]]);
val f = satSolver (And [Var 1, Var 2]);
val g = satSolver (And [Var 1, Or [Var 2]]);
val h = satSolver (And [Or [False, (Var 1)], Var 3]);
val hi = satSolver (And [Or [False, (Not (Var 1))], Var 3]);
val i = satSolver (And [Or [True, Not (Var 1)], Var 3]);

val j = satSolver (And [Or [Var 1, Var 2], Or [Var 3, Var 4, Var 5, Var 6], Var 7, Var 8]);

val t8 = satSolver (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)], Or[Not (Var 3), Var 1]]);


val k = satSolver (Not(Or [Var 1, Var 2])) handle InvalidCNF => (print "InvalidCNF\n"; NONE);
val l = satSolver (Or [And [Var 1, Var 2], Var 3]) handle InvalidCNF => (print "InvalidCNF\n"; NONE);
val m = satSolver (And [(Var 1), Or [Var 2, And [Var 3, Var 4]]]) handle InvalidCNF => (print "InvalidCNF\n"; NONE);


val test9 = satSolver (And [Or [Var 1, Var 3], Or [Not (Var 1), Not (Var 3)],
                        Or[Not (Var 3), Var 1], Or[Not (Var 1), Var 3]]);

val test9reduced = satSolver (
    And [
        Or [Not (Var 3), False],
        Or [Var 3, False]
        ]
    );

val test9reduced' = satSolver (And [Not True,True]);