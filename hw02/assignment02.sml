(* Assignment 2 *)



(********************* Problem 1 *********************) 
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



(********************* Problem 2 *********************) 

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* Problem 2a *)
fun card_color(suit, rank) = 
  case suit of
       Clubs => Black
     | Spades => Black
     | _ => Red


(* Problem 2b *)
fun card_value(suit, rank) = 
  case rank of
       Ace => 11
    |  Queen => 10
    |  King => 10
    |  Jack => 10
    |  Num i => i

(* Problem 2c *)
fun remove_card(cs, c, e) = 
  case cs of
       [] => raise e
     | card::tl => if card = c then tl else
                      card::remove_card(tl, c, e)


(* Problem 2d *)
fun all_same_color(cs) = 
  case cs of
       [] => true
     | _::[] => true
     | card1::(card2::rest) => (card_color card1 = card_color card2  andalso
                             all_same_color(card2::rest))


(* Problem 2e *)
fun sum_cards(cs) = 
  let fun help(cs, acc) =
          case cs of
               [] => acc
             | card::tl => help(tl, acc + (card_value card))
  in
     help(cs, 0)
  end


(* Problem 2f *)
fun score(cs, goal) = 
  let val sum = sum_cards cs
      val same_col = all_same_color cs
      val sgg = sum > goal
  in 
    case (sgg, same_col) of
         (true, true)  => (3*(sum-goal)) div 2
       | (true, false) => (3*(sum-goal))
       |  _ => goal-sum

  end


(* Problem 2g *)
(* Write a function officiate, which “runs a game.” It takes a card list (the
* card-list) a move list (what the player “does” at each point), and an int (the
* goal) and returns the score at the end of the game after processing (some or
* all of) the moves in the move list in order. Use a locally defined recursive
* helper function that takes several arguments that together represent the
* current state of the game.*)
fun officiate(cs, moves, goal) = 











