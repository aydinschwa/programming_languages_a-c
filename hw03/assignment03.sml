(* Assignment 3 *)

exception NoAnswer

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

(* Problem 9 *)

