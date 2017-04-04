(* IF THEN VERSION *)
(* FUNCTION NAME: delnthcworker *)
(* DESCRIPTION: worker for delnth *)
fun delnthcworker(chars:char list, n:int) = if n=1 
					    then tl chars
                       	              	    else hd chars :: delnthcworker(tl chars, n-1);  
(* FUNCTION NAME: delnthc *)
(* DESCRIPTION: From the given string the character at the given index will be deleted *)
fun delnthc(letters:string, n:int) = implode(delnthcworker(explode letters, n));

(* FUNCTION NAME: dispnthcworker *)
(* DESCRIPTION: worker for dispnthc *)
fun dispnthcworker(chars:char list, n:int) = if n=1
				      	     then hd chars 
				      	     else dispnthcworker(tl chars, n-1);
(* FUNCTION NAME: dispnthc *)
(* DESCRIPTION: From the given string the character at the given index will be displayed *)
fun dispnthc(letters:string, n:int) = dispnthcworker(explode letters, n);

(* FUNCTION NAME: multin *)
(* DESCRIPTION: Multiplies the first number from the list by the second number, the third number times. To create a list of all the multiplicative steps *)
fun multin [n:int, mult:int, times:int] = if times = 0
					  then [n]
				          else n::multin[n*mult, mult, times-1];

(* FUNCTION NAME: remv *)
(* DESCRIPTION: Removes all occurences of a given element from a list *)
fun remv(out, L) = if null L
	           then nil
		   else if out = hd L 
			then remv(out, tl L) 
			else hd L::remv(out, tl L);

(* FUNCTION NAME: remvdub *)
(* DESCRIPTION: Removes duplicates from a list *)
fun remvdub(L:string list) = if null L
			     then nil
			     else hd L::remvdub(remv(hd L, tl L));

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
fun reverse(L) = if null L
		 then nil
		 else reverse(tl L)@[hd L];

(* FUNCTION NAME: chr2int *)
(* DESCRIPTION: Turns a single character into its integer representation *)
fun chr2int(c:char) = ((ord c)-48);

(* FUNCTION NAME: str2intworker *)
(* DESCRIPTION: Worker for str2int *)
fun str2intworker(L:char list, past:int) = if null L
		         	  	   then past
			 	  	   else str2intworker(tl L, (past * 10) + (chr2int(hd L)));
(* FUNCTION NAME: str2int *)
(* DESCRIPTION: Changes a string to its integer representation *)
fun str2int(s:string) = str2intworker(explode s, 0);

(* FUNCTION NAME: indeworker *)
(* DESCRIPTION: Worker for inde *)
fun indeworker(e, L, i) = if null L
		       	  then nil
		       	  else (if (hd L) = e 
			  	then i::indeworker(e, (tl L), i+1) 
			  	else indeworker(e, (tl L), i+1));
(* FUNCTION NAME: inde *)
(* DESCRIPTION: Gives the indexes of the given number in the given list *)
fun inde(e,L) = indeworker(e,L,1);

(* FUNCTION NAME: neleworker *)
(* DESCRIPTION: Worker for nele *)
fun neleworker(L,n,i) = if (null L)
			then nil
			else if (i = 1)
			     then (hd L)::neleworker((tl L), n, n) 
			     else (hd L)::neleworker(L, n, i-1);

(* FUNCTION NAME: nele *)
(* DESCRIPTION: Will expand a list to repeat elements n times.*)
fun nele(L,n) = neleworker(L,n,n);

(* FUNCTION NAME: spliatworker *)
(* DESCRIPTION: Worker for spliat *)
fun spliatworker(L:char list, R:char list, pos:int) = if pos = 0 orelse null R
					 	      then [implode L, implode R]
					 	      else spliatworker(L@[(hd R)], tl R, pos-1);

(* FUNCTION NAME: spliat *)
(* DESCRIPTION: Splits a string at a given position. *)
fun spliat(letters:string, pos:int) = spliatworker(nil, explode(letters), pos);

(* FUNCTION NAME: ntrinworker *)
(* DESCRIPTION: Worker for ntrin *)
fun ntrinworker(n,i,c) = if n = 0
		         then nil
		         else (i+c)::ntrinworker(n-1,i+1,c+i);

(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: This function will generate a list of n triangular numbers *)
fun ntrin(n) = ntrinworker(n, 1, 0);

(* FUNCTION NAME: isfactworker *)
(* DESCRIPTION: Worker for isfact. n represents current ammount eg 120. m is count/current test number. n is modded by m as a test then divided by m. if n = 0 then true else false *)
fun isfactworker(n,m)= if n = 1 
	    	       then true
		       else if (n mod m) = 0 
	      	            then isfactworker(n div m, m+1) 
			    else false;

(* FUNCTION NAME: isfact *)
(* DESCRIPTION: This function tests if a number is a factorial *)
fun isfact(n) = if n = 2 orelse n = 1 orelse n = 0
	    	then true
 	    	else isfactworker(n,2);

(* FUNCTION NAME: inc *)
(* DESCRIPTION: Increments the second number of a tuple if two integers *)
fun inc(T:(int * int)) = (#1 T,(#2 T) + 1);

(* FUNCTION NAME: incornew *)
(* DESCRIPTION: Checks a list of 2 int tuples first elements for n. If found the second element of the tuple is incremented. If not, a new tuple with n and one is added.  *)
fun incornew (n:int, L:(int * int) list) = if null L 
		   		           then [(n, 1)]
		 		       	   else if (n = (#1 (hd L)))
		      		      	         then  inc (hd L)::(tl L)
		      		       	         else (hd L)::incornew(n, tl L);

(* FUNCTION NAME: occrworker *)
(* DESCRIPTION: Worker for occr *)
fun occrworker(L, C) = if null L
			  then C
                          else occrworker(tl L, incornew((hd L), C));

(* FUNCTION NAME: occr *)
(* DESCRIPTION: Counts occurences of an element in a list *)
fun occr(L) = occrworker(L, nil);

(* FUNCTION NAME: insfront *)
(* DESCRIPTION: Creates a function that will insert the given element into a list *)
fun insfront(n) = fn L => n::L;

(* FUNCTION NAME: map *)
(* DESCRIPTION: Simple map function that will apply a function to all the elmenents of a list, returning the modified list. *)
fun map(func, L) = if null L
		   then nil
		   else (func(hd L))::map(func, tl L); 

(* FUNCTION NAME: inseach *)
(* DESCRIPTION:   inserts an element into every possible location in a list *)
fun inseach(n, L) = if null L
		    then [[n]]
		    else (n::L)::(map(insfront(hd L), inseach(n, tl L)));

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
