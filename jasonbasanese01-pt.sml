(* PATTERN MATCHING VERSION *)
(* FUNCTION NAME: delnthcworker *)
(* DESCRIPTION: Worker for delnthc. Uses the exploded string list.*)
fun delnthcworker(nil, n) = nil
|   delnthcworker(x::xs, 1) = xs
|   delnthcworker(x::xs, n) = x::delnthcworker(xs, n-1);

(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: From the given string the character at the given index will be deleted *)
fun delnthc(letters:string, n:int) = implode(delnthcworker(explode letters, n));

(* FUNCTION NAME: dispnthcworker *)
(* DESCRIPTION: Worker for dispnthc *)
fun dispnthcworker(x::xs, 1) = x
|   dispnthcworker(x::xs, n) = dispnthcworker(xs, n-1)

(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: From the given string the character at the given index will be displayed *)
fun dispnthc(letters:string, n:int) = dispnthcworker(explode letters, n);

(* FUNCTION NAME: multin *)
(* DESCRIPTION: Multiplies the first number from the list by the second number, the third number times. To create a list of all the multiplicative steps *)
fun multin[n, mult, 0] = [n]
|   multin[n, mult, times] = n::multin[n*mult, mult, times-1];
(* FUNCTION NAME: remv *)
(* DESCRIPTION: Removes all occurences of a given element from a list *)
fun remv(out, nil) = nil
|   remv(out, x::xs) = if out = x then remv(out, xs)else x::remv(out, xs);

(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: Removes duplicates from a list *)
fun remvdub(nil) = nil
|   remvdub(x::xs) = x::remvdub(remv(x, xs));

(* FUNCTION NAME: digit2str *)
(* DESCRIPTION: Turns a single digit to a string *)
fun digit2str(n:int) = str (chr (n + 48));

(* FUNCTION NAME: int2str *)
(* DESCRIPTION: Turns an integer into its string representation *)
fun int2str(n:int) = if (n div 10) = 0
		     then digit2str(n)
		     else int2str(n div 10) ^ digit2str(n mod 10);

(* FUNCTION NAME: reverse *)
(* DESCRIPTION: Reverses the order of elements in a list *)
fun reverse(nil) = nil
|   reverse(x::xs) = reverse(xs)@[x];

(* FUNCTION NAME: chr2int *)
(* DESCRIPTION: changes a single char to an int*)
fun chr2int(c:char) = ((ord c)-48);

(* FUNCTION NAME: str2intworker *)
(* DESCRIPTION: worker for str2int*)
fun str2intworker(nil, past) =  past
|   str2intworker(x::xs, past) = str2intworker(xs, (past * 10) + (chr2int(x)));

(* FUNCTION NAME: str2int *)
(* DESCRIPTION: Changes a string to its integer representation *)
fun str2int(s:string) = str2intworker(explode s, 0);

(* FUNCTION NAME: indeworker *)
(* DESCRIPTION: Worker for inde *)
fun indeworker(e, nil, i) = nil
|   indeworker(e, x::xs, i) = if x = e 
			      then i::indeworker(e, xs, i+1) 
			      else indeworker(e, xs, i+1);

(* FUNCTION NAME: inde *)
(* DESCRIPTION: Gives the indexes of the given number in the given list *)
fun inde(e,L) = indeworker(e,L,1);

(* FUNCTION NAME: neleworker *)
(* DESCRIPTION: worker for nele function *)
fun neleworker(nil,n,i) = nil
|   neleworker(x::xs,n,1) = x::neleworker(xs, n, n) 
|   neleworker(x::xs,n,i) = x::neleworker(x::xs, n, i-1);
			    
(* FUNCTION NAME: nele *)
(* DESCRIPTION: Will expand a list to repeat elements n times.*)
fun nele(L,n) = neleworker(L,n,n);

(* FUNCTION NAME: spliatworker *)
(* DESCRIPTION: Worker for spliat *)
fun spliatworker(L:char list, R:char list, 0) = [implode L, implode R]
|   spliatworker(L:char list, nil, pos:int) = [implode L]
|   spliatworker(L:char list, r::rs, pos:int) = spliatworker(L@[r], rs, pos-1);

(* FUNCTION NAME: spliat *)
(* DESCRIPTION: Splits a string at a given position. *)
fun spliat(letters:string, pos:int) = spliatworker(nil, explode(letters), pos);

(* FUNCTION NAME: ntrinworker *)
(* DESCRIPTION: worker for ntrin *)
fun ntrinworker(0,i,c) = nil
|   ntrinworker(n,i,c) = (i+c)::ntrinworker(n-1,i+1,c+i);

(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: This function will generate a list of n triangular numbers *)
fun ntrin(n) = ntrinworker(n, 1, 0);

(* FUNCTION NAME: isfactworker *)
(* DESCRIPTION: Worker for isfact. n represents current ammount eg 120. m is count/current test number. n is modded by m as a test then divided by m. if n = 0 then true else false *)
fun isfactworker(n,m)= let
			  val containsfactor  = (n mod m) = 0
			  val divbyall = n = 1 
			  val factorremoved = n div m
			  val nextfactor = m+1
		       in 
			  if divbyall 
			  then true 
			  else if containsfactor 
			       then isfactworker(factorremoved, nextfactor) 
			       else false
		       end;

(* FUNCTION NAME: isfact *)
(* DESCRIPTION: This function tests if a number is a factorial *)
fun isfact(0) = true
|   isfact(1) = true
|   isfact(2) = true
|   isfact(n) = isfactworker(n,2);

(* FUNCTION NAME: incornew *)
(* DESCRIPTION: If integer n is in the list of element counts, the count of that element is increased. Otherwise a new element with count one is added to the list *)
fun incornew(n:int, nil) =  [(n,1)]
|   incornew(n:int, (x,y)::xs) = if n = x
				    then (x, y+1)::xs else (x, y)::incornew(n, xs);

(* FUNCTION NAME: occr *)
(* DESCRIPTION: Counts occurences of an element in a list *)
(*fun occr(L) = occrworker(L, nil);*)
fun occr(nil) = nil
|   occr(x::xs) = incornew(x, occr(xs));

(* FUNCTION NAME: insfront *)
(* DESCRIPTION: Creates a function that will insert the given element into a list *)
fun insfront(n) = fn L => n::L;

(* FUNCTION NAME: map *)
(* DESCRIPTION: Simple map function that will apply a function to all the elmenents of a list, returning the modified list. *)
fun map(func, nil) = nil
|   map(func, x::xs) = (func(x))::map(func, xs);

(* FUNCTION NAME: inseach *)
(* DESCRIPTION:   inserts an element into every possible location in a list *)
fun inseach(n, nil) = [[n]]
|   inseach(n, x::xs) = (n::x::xs)::(map(insfront(x), inseach(n, xs)));

delnthc("abcdef", 4);
dispnthc("abcdef", 4);
multin([2,3,5]);
remv("a", ["a", "b", "a", "c"]);
remvdub(["a", "b", "a", "c", "b", "a"]);
int2str(1234);
str2int("1234");
inde(1, [1, 2, 1, 1, 2, 2, 1]);
nele([1,2], 3);
spliat("Program", 3);
ntrin(7);
isfact(120);
occr([1,2,1,2,3,2]);
map(insfront(1), [[1,2], nil, [3]]);
inseach(4, [1,2,3]);
