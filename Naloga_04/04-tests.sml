reduce (fn z => fn x => z+x) 0 [1,2,3] = 6;
squares [1,2,3,4,5] = [1,4,9,16,25];
onlyEven [0,1,2,3,4,5,6] = [0,2,4,6];
bestString (fn (a,b) =>  case String.compare(a,b) of
                                GREATER => true
                                | _ => false ) ["a", "b", "c"] = "c";
largestString ["a", "b", "c", "d", "a"] = "d";
longestString ["", "a", "ab", "abc", "d"] = "abc";
quicksort 
    (fn (a,b) => if a > b then GREATER else LESS)
    [5,4,3,2,1,9,10,6,7,8] 
        = [1,2,3,4,5,6,7,8,9,10];
dot [1,2,3] [1,2,3] = 14;
dot [1,2,3] [] = 0;
transpose [[1,2,3],[4,5,6],[7,8,9]] = [[1,4,7],[2,5,8],[3,6,9]];
transpose [[3,1,2],[~2,0,5]] = [[3,~2],[1,0],[2,5]];
multiply [[~1,3],[0,5],[2,5]] [[3,1,2],[~2,0,5]] = [[~9,~1,13],[~10,0,25],[~4,2,29]];
group [1,1,1,1,2,2,3,1,2,3] = [(1,4),(2,2),(3,1),(1,1),(2,1),(3,1)];
group ["a","a","b","a","c","c","a"] = [("a",2),("b",1),("a",1),("c",2),("a",1)];