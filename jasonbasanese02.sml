(* FUNCTION NAME: startMatch *)
(* DESCRIPTION: Checks if the first list matches the start of the second list. c1 and c2 mean the char or chars of the first and second string. *)
fun startMatch(nil, _) = true
|   startMatch(_, nil) = false
|   startMatch(c1::c1s, c2::c2s) = if c1 = c2 then startMatch(c1s, c2s) else false;

(* FUNCTION NAME: subStringWorker *)
(* DESCRIPTION: returns index of string if it is contained in second string, otherwise it returns ~1*)

fun subStringWorker (nil, nil, _) = 0
|   subStringWorker (_, nil, _) = ~1
|   subStringWorker (c1s, c2s, index) = if startMatch(c1s, c2s) then index else subStringWorker(c1s, tl c2s, index + 1);

(* FUNCTION NAME: subString *)
(* DESCRIPTION: passes the strings to subStringWorker in exploded form.*)
fun subString (str1, str2) = subStringWorker(explode(str1), explode(str2), 0);

(* FUNCTION NAME: getSums *)
(* DESCRIPTION: will get all the numbers that are the sum between each adjacent number in the list*)
fun getSums [n] = nil
|   getSums (x::y::zs) = (x + y)::(getSums(y::zs));

(* FUNCTION NAME: reverse *)
(* DESCRIPTION: reverses elements of a list *)
fun reverse(nil) = nil
|   reverse(x::xs) = reverse(xs)@[x];

(* FUNCTION NAME: pTrianglesWorker *)
(* DESCRIPTION: Gives n rows of Pascals Triangle.in reverse order*)
fun pTrianglesWorker(1) = [[1]]
|   pTrianglesWorker(n) = let 
			val prev = (hd (pTrianglesWorker(n-1))) 
			val next = 1::getSums(prev)@[1] 
		    in 
			next::pTrianglesWorker(n-1)
		    end;
(* FUNCTION NAME: pTriangles *)
(* DESCRIPTION: Gives n rows of Pascals Triangle.in reverse order*)
fun pTriangles(n) = reverse(pTrianglesWorker(n));

(* FUNCTION NAME: incornew *)
(* DESCRIPTION: If integer n is in the list of element counts, the count of that element is increased. Otherwise a new element with count one is added to the list *)
fun incornew(n, nil) =  [(n,1)]
|   incornew(n, (x,y)::xs) = if n = x
                                    then (x, y+1)::xs else (x, y)::incornew(n, xs);

(* FUNCTION NAME: occr *)
(* DESCRIPTION: Counts occurences of an element in a list *)
fun occr(nil) = nil
|   occr(x::xs) = incornew(x, occr(xs));

(* FUNCTION NAME: modeLWorker *)
(* DESCRIPTION: Finds the mode of a list, or returns all tied counts if there is a tie. Gets passed occurences, aswell as starting values. Looks for max or maxes.*)
fun modeLWorker(nil, _, maxes) = maxes
|   modeLWorker((item,count)::xs, max, maxes) = if count > max 
                                                then modeLWorker(xs, count, [(item,count)])
                                                else if count = max
                                                     then modeLWorker(xs, max, (item,count)::maxes)
                                                     else modeLWorker(xs, max, maxes); 
(* FUNCTION NAME: modeL *)
(* DESCRIPTION: Passes occurences count to modeLWorker, aswell as a starting max of 0 and empty max list*)
fun modeL(L) = modeLWorker(occr(L), 0, nil);
(* FUNCTION NAME: insfront *)
(* DESCRIPTION: Creates a function that will insert the given element into a list *)
fun insfront(n) = fn L => n::L;

(* FUNCTION NAME: map *)
(* DESCRIPTION: Simple map function that will apply a function to all the elmenents of a list, returning the modified list. *)
fun map(func, nil) = nil
|   map(func, x::xs) = (func(x))::map(func, xs);

(* FUNCTION NAME: reduce *)
(* DESCRIPTION: Takes a starting element and applies the given function to that starting element and the first element of the list. Then uses the result as the start for the next step with the second element of the list and so on returning the built value. *)
fun reduce(_, built, nil) = built
|   reduce(func, start, x::xs) = reduce(func, func(start, x), xs);

(* FUNCTION NAME: inseach *)
(* DESCRIPTION:   inserts an element into every possible location in a list *)
fun inseach(n, nil) = [[n]]
|   inseach(n, x::xs) = (n::x::xs)::(map(insfront(x), inseach(n, xs)));

(* FUNCTION NAME: into *)
(* DESCRIPTION: puts the elements of one list into another, much like append *)
fun into(L, nil) = L
|   into(L, x::xs) = x::(into(L, xs));

(* FUNCTION NAME: permu *)
(* DESCRIPTION:  Generates all permutations of the identity list 1 to the given variable n *) 
fun permu(1) = [[1]]
|   permu(n) = reduce(into, nil, map(fn L => inseach(n, L), permu(n-1)));

(* FUNCTION NAME: smallestToStart *)
(* DESCRIPTION: Moves the smallest element of a list to the start. Builds a list of elements that are not the smallest then takes the last element that might be  
* the smallest and inserts as smallest *)
fun smallestToStart(ns, [s]) = s::ns
  | smallestToStart(ns, m1::m2::ms) = if m1 > m2 then smallestToStart(m1::ns, m2::ms) else smallestToStart(m2::ns, m1::ms);

(* FUNCTION NAME: ssort *)
(* DESCRIPTION:  Sorts a list in ascending order *) 
fun ssort(nil) = nil 
  | ssort([b]) = [b]
  | ssort(L) = let val firstSorted = smallestToStart(nil, L)
               in (hd firstSorted)::ssort(tl firstSorted)
               end;

(* FUNCTION NAME: filter *)
(* DESCRIPTION:  Simple filter function take a function and a list. 
* If the function is true for an element of the list it will stay in, otherwise it will be removed. *) 
fun filter(_, nil) = nil
  | filter(func, x::xs) = if func(x) 
                          then x::filter(func, xs) 
                          else filter(func, xs);

(* FUNCTION NAME: notFactor *)
(* DESCRIPTION: Higher order function that is passed a number and returns a function that checks if its input is not divisible by the given number *)
fun notFactor(n) = fn nq => nq mod n <> 0;

(* FUNCTION NAME: range *)
(* DESCRIPTION: Takes a number and gives a list from two through that number *)
fun range(1) = nil
  | range(n) = n::range(n-1);

(* FUNCTION NAME:  plistHelper *)
(* DESCRIPTION: Worker for plist that takes out any number that is divisible by a prime *)
fun plistHelper(nil) = nil
  | plistHelper(n::ns) = n::(plistHelper(filter(notFactor(n), ns)));

(* FUNCTION NAME:  plist *)
(* DESCRIPTION: Takes a number and returns the list of primes through that number *)
fun plist(1) = nil
  | plist(n) = plistHelper(reverse(range(n)));

subString("bc", "abcabc");
subString("aaa", "aaa");
subString("bc", "ABC");

pTriangles(3);
pTriangles(5);

modeL([1,1,2,3,5,8]);
modeL([1,3,5,2,3,5]);
modeL([true,false,true,true]);

permu(2);
permu(3);

ssort([64,25,12,22,11]);

plist(20);
