(* Problem 1 *)
(* Write a function only_capitals that takes a string list and returns a string
* list that has only the strings in the argument that start with an uppercase
* letter. Assume all strings have at least 1 character. Use List.filter,
* Char.isUpper, and String.sub to make a 1-2 line solution. *)
val only_capitals = List.filter(fn x => Char.isUpper (String.sub (x, 0))) 


(* Problem 2 *)
(* Write a function longest_string1 that takes a string list and returns the
longest string in the list. If the list is empty, return "". In the case of a
tie, return the string closest to the beginning of the list. Use foldl,
String.size, and no recursion (other than the implementation of foldl is
recursive). *)
val longest_string1 = List.foldl(fn (x, acc)=>if String.size acc < String.size x
                                              then x
                                              else acc ) ""


(* Problem 3 *)
(* Write a function longest_string2 that is exactly like longest_string1 except
* in the case of ties it returns the string closest to the end of the list. Your
* solution should be almost an exact copy of longest_string1. Still use foldl
* and String.size. *)
val longest_string2 = List.foldl(fn(x,acc)=>if String.size acc <= String.size x
                                           then x else acc ) ""


(* Problem 4 *)
(* longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and
longest_string2 but is more general because it takes a function as an argument.
*)
fun longest_string_helper f strs =
    List.foldl (fn (x, acc) => if (f (x,acc)) then x else acc) "" strs 

val longest_string3 = longest_string_helper (fn (x,y) => String.size x >
                                                         String.size y)

val longest_string4 = longest_string_helper (fn (x,y) => String.size x >= 
                                                         String.size y)


(* Problem 5 *)
(* Write a function longest_capitalized that takes a string list and returns the
* longest string in the list that begins with an uppercase letter, or "" if
* there are no such strings. Assume all strings have at least 1 character. Use a
* val-binding and the ML library’s o operator for composing functions. Resolve
* ties like in problem 2. *)
val longest_capitalized = longest_string2 o only_capitals


(* Problem 6 *)
(* Write a function rev_string that takes a string and returns the string that
* is the same characters in reverse order. Use ML’s o operator, the library
* function rev for reversing lists, and two library functions in the String
* module. (Browse the module documentation to find the most useful functions.)
* *)
val rev_string = String.implode o List.rev o String.explode

(* Coursera Programming Languages, Homework 3, Provided Code *)

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


(* Problem 7 *)
fun first_answer f li =
    case li of
         [] => raise NoAnswer 
       | el::tl => case (f el) of
                        NONE => first_answer f tl
                      | SOME v => v


(* Problem 8 *)
fun all_answers f li =
    let fun aux f li acc =
            case li of
                 [] => SOME acc 
               | v::tl => case (f v) of
                               NONE => NONE
                             | SOME v2 => aux f tl (v2@acc)
    in
        aux f li []
    end
                      
                      
(* Problem 9 *)

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
