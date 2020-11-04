(* Assignment 2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* Problem 1a *) 
fun all_except_option(name, lon) = 
  case lon of
       [] => NONE
     | lname::tl => if same_string(lname, name)
                    then SOME tl
                    else case all_except_option(name, tl) of
                              NONE => NONE
                            | SOME nm => SOME(lname::nm)


(* Problem 1b *)
fun get_substitutions1(lolon, name) = 
  case lolon of
       [] => []
    |  lon::tl => let val nlist = all_except_option(name, lon)
                  in case nlist of
                          NONE => get_substitutions1(tl, name)
                        | SOME nlist => nlist@get_substitutions1(tl, name)
                  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

