(* Assignment 3 *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


(* Problem 1 *)
fun only_capitals(strs) = 
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs

(* Problem 2 *)
fun longest_string1(strs) = 
    foldl (fn (x, acc) => if (String.size x) > (String.size acc) then x else acc ) "" strs

(* Problem 3 *)
fun longest_string2(strs) = 
    foldl (fn (x, acc) => if (String.size x) >= (String.size acc) then x else acc ) "" strs
    
(* Problem 4 *)
fun longest_string_helper func strs = 
    foldl (fn (x, acc) => if func((String.size x), (String.size acc)) then x else
      acc) "" strs

fun longest_string3 strs = 
    let val func = fn(x, acc) => if x > acc then true else false 
    in longest_string_helper func strs
    end

fun longest_string4 strs = 
    let val func = fn(x, acc) => if x >= acc then true else false 
    in longest_string_helper func strs
    end

(* Problem 5 *)
fun longest_capitalized(strs) = 
    (longest_string1 o only_capitals) strs

(* Problem 6 *)
fun rev_string(str) = 
    (String.implode o List.rev o String.explode) str    

(* Problem 7 *)
fun first_answer func some_list = 
    case some_list of
         []       => raise NoAnswer
       | elem::tl => if isSome (func elem)
                     then func elem 
                     else first_answer func tl


(* Problem 8 *)
fun all_answers func some_list =
    let
        fun all_help some_list acc =
            case some_list of
                 []       => SOME acc 
               | elem::tl => let val ans = func elem
                             in 
                                if null ans then NONE
                                else all_help tl (ans@acc)
                             end                             
    in
        all_help some_list [] 
    end


(******************** Next Section *******************)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(* Problem 9a *)
fun count_wildcards(pat) = 
    g (fn x => 1) (fn x => 0) pat


(* Problem 9b *)
fun count_wild_and_variable_lengths(pat) = 
    g (fn x => 1) (fn x => String.size x) pat


(* Problem 9c *)
fun count_some_var(str, pat)= 
    g (fn x => 0) (fn x => if x = str then 1 else 0) pat


(* Problem 10 *)
(* Write a function check_pat that takes a pattern and returns true if and only
* if all the variables appearing in the pattern are distinct from each other
* (i.e., use different strings). The constructor names are not relevant. Hints:
* The sample solution uses two helper functions. The first takes a pattern and
* returns a list of all the strings it uses for variables. Using foldl with a
* function that uses @ is useful in one case. The second takes a list of strings
* and decides if it has repeats. List.exists may be useful. Sample solution is
* 15 lines. These are hints: We are not requiring foldl and List.exists here,
* but they make it easier. *)
fun check_pat(pat) = 
    let
        fun string_list acc



