exception NoItem
exception Mismatch

fun length  [] = 0									(*helper function for mulltiple questions, determines the length of a list in int*)
		| length (x::xs) = 1 + length xs;

(*Question no.1*)
fun avg_list []=raise NoItem
|   avg_list list = 
 let
 	
 	fun lenR [] = 0.0									(*helper function, length in real*)
		| lenR (x::xs) = 1.0 + lenR xs;					

	fun sumR list=								    (*helper function, sum of list elements in real*)
	if null list
	then 0.0
	else (hd list) + (sumR (tl list));
 
in 
 	sumR list / lenR list
 end;

 (*Question 2*)
 fun get_index [] _ = raise NoItem
|	get_index (x::xs) 0 = x
|	get_index (x::xs) n = get_index xs (n-1) ;

(*Question 3*)
fun get_odd_midpoint odd_list = 
	let	
		val mid = (length odd_list +1) div 2 -1;		(*middle element*)
	in
		get_index odd_list mid
	end;

(*Question 4*)
fun get_even_midpoint even_list = 
	let	
		val mid_l= (length even_list +1) div 2 -1;		(*middle element*)
		val mid_h= mid_l+1								(*As this is a list with even number of elements, we need to consider mid and mid+1 elements and take their average to find the midpoint*)
	in
		((get_index even_list mid_l) + (get_index even_list mid_h))/2.0
	end;

(*Question 5*)
fun get_median list=
let
fun  quicksort op < [] = []									(*Quicksort from lecture slides*)
| quicksort op < [x] = [x]
| quicksort op < (a::bs) =
let  fun  partition (left , right , []) = (left , right)
| partition (left , right , x::xs) =
if x < a
then  partition (x::left , right , xs)
else  partition (left , x::right , xs)
val (left , right) = partition  ([], [], bs)
in
quicksort op< left @ (a ::  quicksort op < right)
end;
in
	if null list
	then raise NoItem
	else 
		if (length list) mod 2 = 0								(*To check if the list has even number of elements or odd*)
		then get_even_midpoint (quicksort op< list)				(*Median can be found by getting midpoint of sorted list using quicksort and midpoint functions defined above*)
		else get_odd_midpoint (quicksort op< list)
end;

(*Question 6*)
fun listsum [] _ = raise NoItem
| listsum list num=
let
fun sum list=								(*helper function to find the sum of elements of an integer list*)
	if null list
	then 0
	else (hd list) + (sum (tl list));
in
	if (sum list) = num 	       			(*Checking if the sum is equal to the given number*)
	then true
	else false
end;

(*Question 7*)
fun isten [] = raise NoItem
| isten list=
let
	fun flip f y x = f x y;					(*flip function defined to get the first argument of listsum to be the number you want to check the sum with*)
	val checkTen= flip listsum 10;			(*Partial application to check if the sum of any list is ten*)
in
	checkTen list 							(*Passing the argument list to the function*)
end;

(*Question 8*)
fun zip ([],[]) = []
| zip (list1,list2)=
if length list1 = length list2								(*Proceed only if the lengths match*)
then
	if length list1 > 1												
	then (hd list1, hd list2)::zip (tl list1, tl list2)		(*Zipping heads of the two list and placing the tuple as the first element of the new list. Further tuples are returned by recursive zip call*)
	else [(hd list1, hd list2)]								(*If there is only one element in each lists, we simply form a tuple which becomes the only element in the new list*)
else raise Mismatch;

(*Question 9*)
fun unzip [] = ([],[])									
  | unzip ((x,y)::rest)=									(*Pattern for multiple tuples. 'rest' signifies all tuples after the first*)
      let 
      val (rest_x,rest_y) = unzip rest;	 					(*Recursive call to unzip the rest of the tuples in the list*)
      in 
      (x::rest_x,y::rest_y)									(*concatenating the unzipped results*)
      end;

(*Question 10*)
fun plus x y =x+y; 											(*function to use for F as an example. You can define any other function to use for F.*)

fun scan_left F y [] = [y]
|scan_left F y (x::xs) =
 y::(scan_left F (F y x) xs)	;

(*Question 11*)
fun fact_list 0 = raise NoItem
| fact_list n= 
let

fun factorial n= 								(*helper function to find factorial of a number*)
if n<=1
then 1
else (factorial (n-1))*n;

fun countup n=									(*helper function to create a list of integers from 1 to n*)
let
	fun countup' 0 l = l
	| countup' i l =countup' (i-1) (i::l);
in
	countup' n []
end;

in
map factorial (countup n)						(*applying factorial to each element in the list consisting of 1 upto n.*)
end;