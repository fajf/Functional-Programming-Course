val neg0 = neg Zero = Zero;
val neg1 = neg (Pred(Zero)) = Succ(Zero);
val neg2 = neg (Succ(Zero)) = Pred(Zero);
val neg3 = neg (Succ(Succ(Succ(Zero)))) = Pred(Pred(Pred(Zero)));
val neg4 = neg (Succ(Succ(Succ(Pred(Succ(Pred(Succ(Zero)))))))) = Pred(Pred(Pred(Zero)));

val add0 = add (Zero, Zero) = Zero;
val add1 = add (Pred(Zero), Succ(Zero)) = Zero;
val add2 = add (Succ(Zero), Pred(Zero)) = Zero;
val add3 = add (Succ(Succ(Succ(Zero))), Pred(Pred(Pred(Zero)))) = Zero;
val add4 = add (Pred(Zero), Succ(Succ(Succ(Pred(Succ(Pred(Succ(Zero)))))))) = Succ(Succ(Zero));

val comp0 = comp (Zero, Zero) = EQUAL;
val comp1 = comp (Pred(Zero), Succ(Zero)) = LESS;
val comp2 = comp (Succ(Zero), Pred(Zero)) = GREATER;
val comp3 = comp (Succ(Succ(Succ(Zero))), Pred(Pred(Pred(Zero)))) = GREATER;
val comp4 = comp (Pred(Zero), Succ(Succ(Succ(Pred(Succ(Pred(Succ(Zero)))))))) = LESS;

val contains0 = contains ((Leaf 1), 1) = true;
val contains1 = contains ((Node(1, Leaf 2, Leaf 3)), 1) = true;
val contains2 = contains ((Node(1, Leaf 2, Leaf 3)), 3) = true;
val contains3 = contains (((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))), 6) = true;
val contains4 = contains (((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))), 13) = false;

val countLeaves0 = countLeaves (Leaf 1) = 1;
val countLeaves1 = countLeaves (Node(1, Leaf 2, Leaf 3)) = 2;
val countLeaves2 = countLeaves (Node(1, Leaf 2, Leaf 3)) = 2;
val countLeaves3 = countLeaves ((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))) = 4;
val countLeaves4 = countLeaves ((Node(1, Leaf 2, Node(3, Node(4, Node (6, Leaf 10, Leaf 11), Leaf 7), Leaf 5)))) = 5;

val countBranches0 = countBranches (Leaf 1) = 0;
val countBranches1 = countBranches (Node(1, Leaf 2, Leaf 3)) = 2;
val countBranches2 = countBranches (Node(1, Leaf 2, Leaf 3)) = 2;
val countBranches3 = countBranches ((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))) = 6;
val countBranches4 = countBranches ((Node(1, Leaf 2, Node(3, Node(4, Node (6, Leaf 10, Leaf 11), Leaf 7), Leaf 5)))) = 8;

val height0 = height (Leaf 1) = 1;
val height1 = height (Node(1, Leaf 2, Leaf 3)) = 2;
val height2 = height (Node(1, Leaf 2, Leaf 3)) = 2;
val height3 = height ((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))) = 4;
val height4 = height ((Node(1, Leaf 2, Node(3, Node(4, Node (6, Leaf 10, Leaf 11), Leaf 7), Leaf 5)))) = 5;

val toList0 = toList (Leaf 1) = [1];
val toList1 = toList (Node(1, Leaf 2, Leaf 3)) = [2,1,3];
val toList2 = toList (Node(1, Leaf 2, Leaf 3)) = [2,1,3];
val toList3 = toList ((Node(1, Leaf 2, Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))) = [2,1,6,4,7,3,5];
val toList4 = toList ((Node(1, Leaf 2, Node(3, Node(4, Node (6, Leaf 10, Leaf 11), Leaf 7), Leaf 5)))) = [2,1,10,6,11,4,7,3,5];

val isBalanced0 = isBalanced (Leaf 1) = true;
val isBalanced1 = isBalanced (Node(1, Leaf 2, Leaf 3)) = true;
val isBalanced2 = isBalanced (Node(1, Leaf 2, Node(3, Leaf 4, Leaf 5))) = true;
val isBalanced3 = isBalanced ((Node(1, Node (2, Leaf 8, Leaf 9), Node(3, Node(4, Leaf 6, Leaf 7), Leaf 5)))) = true;
val isBalanced4 = isBalanced ((Node(1, Node (2, Node(8, Leaf 12, Leaf 13), Leaf 9), Node(3, Node(4, Node (6, Leaf 10, Leaf 11), Leaf 7), Leaf 5)))) = false;

val isBST0 = isBST (Node(2, Leaf 1, Leaf 3)) = true;
val isBST1 = isBST (Node(2, Leaf 1, Node(3, Leaf 5, Leaf 4))) = false;
val isBST2 = isBST (Node(10, Node(5, Node(3, Leaf 2, Leaf 4), Leaf 8), Node(20, Node(15, Leaf 11, Leaf 17), Leaf 25))) = true;
val isBST2 = isBST (Node(10, Node(5, Node(3, Leaf 2, Leaf 4), Leaf 8), Node(20, Node(15, Leaf 11, Leaf 22), Leaf 25))) = false;

OS.Process.exit OS.Process.success;