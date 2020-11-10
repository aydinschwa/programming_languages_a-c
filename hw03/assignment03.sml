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
fun check_pat(pat) = 
    let fun str_list(pat, acc) =
            case pat of
                Variable v => v::acc  
              | TupleP ps  => List.foldl str_list acc ps
              | ConstructorP(_,p) => str_list(p, acc)
              | _ => []
    
        fun repeats(strlist, acc) = 
            case strlist of
                 [] => acc
               | str::tl => if List.exists (fn x => x = str) acc
                            then repeats(tl, acc)
                            else repeats(tl, str::acc)

        val all_names = str_list(pat, [])
        val sub_names = repeats(all_names, [])
    in
      if List.length all_names = List.length sub_names
      then true
      else false 
    end


(* Problem 11 *)
(* Write a function match that takes a valu * pattern and returns a (string *
* valu) list option, namely NONE if the pattern does not match and SOME lst
* where lst is the list of bindings if it does. Note that if the value matches
* but the pattern has no patterns of the form Variable s, then the result is
* SOME []. Hints: Sample solution has one case expression with 7 branches. The
* branch for tuples uses all_answers and ListPair.zip. Sample solution is 13
* lines. Remember to look above for the rules for what patterns match what
* values, and what bindings they produce. These are hints: We are not requiring
* all_answers and ListPair.zip here, but they make it easier. *)




