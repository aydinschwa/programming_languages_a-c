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


(* Problem 1c *)
fun get_substitutions2(lolon, name) =
  let fun subtl (lolon, name, acc) =
  case lolon of
       [] => acc
    |  lon::tl => let val nlist = all_except_option(name, lon)
                  in case nlist of
                          NONE => subtl(tl, name, acc)
                        | SOME nlist => subtl(tl, name, acc@nlist)
                  end

  in subtl(lolon, name, [])

  end

(* Problem 1d *)
(* feels pretty inelegant. try to fix later *)
fun similar_names(lolon, {first=f, middle=m, last=l}) = 
  let 
      val first= [{first=f, middle=m, last=l}]
      val lon = get_substitutions1(lolon, f) 
      fun mklist(lon, {first=f, middle=m, last=l}) =
        case lon of
             [] => []
           | name::tl => ({first=name, middle=m, last=l})::mklist(tl, {first=f,
                           middle=m, last=l})
  in case lon of
          [] => []
        | name::_ => first@mklist(lon, {first=name, middle=m, last=l})
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

