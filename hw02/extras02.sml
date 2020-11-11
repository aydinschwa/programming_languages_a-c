type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* Problem 1 *)
fun pass_or_fail (grade, id) =
    case (id, grade) of
         (_, SOME i) => if i >= 75 then pass else fail
       | (_, _) => fail


(* Problem 2 *)
fun has_passed (grade, id) =
    case pass_or_fail (grade, id) of
         pass => true
       | fail => false


(* Problem 3 *)
fun number_passed fg_list =
    case fg_list of
         [] => 0
       | fg::tl => if has_passed (fg) 
                   then 1 + number_passed tl
                   else number_passed tl


(* Problem 4 *)
fun number_misgraded pf_fg_li =
    case pf_fg_li of
         (pass::tl1, fg::tl2) => if has_passed (fg) 
                                 then number_misgraded (tl1, tl2)
                                 else 1 + number_misgraded (tl1, tl2)
       | (fail::tl1, fg::tl2) => if has_passed (fg)
                                 then 1 + number_misgraded (tl1, tl2)
                                 else number_misgraded (tl1, tl2)
       | (_, _) => 0


(****************** Next Section ******************)
datatype 'a tree = leaf
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* Problem 5 *)
fun tree_height tree =
    case tree of
         leaf => 0
       | node {value, left, right} => 
           1 + ((fn (x,y) => if x > y then x else y) (tree_height left,
           tree_height right))


(* Problem 6 *)
fun sum_tree tree =
    case tree of
         leaf => 0
       | node {value, left, right} => value + sum_tree left + sum_tree right


(* Problem 7 *)
fun gardener tree =
    case tree of
         leaf => leaf
       | node {value, left, right} => if value = prune_me
                                      then leaf 
                                      else node {value = value, left = 
                                                (gardener left), right =
                                                (gardener right)}


(* Problem 8 *)
(* Skipping for now *)



(****************** Next Section ******************)
datatype nat = ZERO | SUCC of nat
exception Negative

(* Problem 9 *)
fun is_positive nat =
    case nat of
         ZERO => false
       | _ => true


(* Problem 10 *)
fun pred nat =
    case nat of
         ZERO => raise Negative
       | SUCC nat => nat


(* Problem 11 *)
fun nat_to_int nat =
    case nat of
         ZERO => 0
       | SUCC nat => 1 + nat_to_int nat


(* Problem 12 *)
fun int_to_nat num =
    if num = 0
    then ZERO
    else SUCC (int_to_nat (num-1))


(* Problem 13 *)
fun add nat1 nat2 =
    case nat1 of
          ZERO => nat2
       |  SUCC nat => SUCC (add nat nat2)


(* Problem 14 *)
fun sub nat1 nat2 =
    case nat2 of
        ZERO => nat1
      | SUCC nat => sub (pred nat1) nat


(* Problem 15 *)
fun mult nat1 nat2 =
    let
        fun aux nat1 nat2 acc =
            case nat2 of
                 ZERO => acc
               | SUCC nat => aux nat1 nat (add acc nat1)
    in
        aux nat1 nat2 ZERO
    end


(* Problem 16 *)
fun less_than (nat1, nat2) =
    case (nat1, nat2) of
         (ZERO, ZERO) => false
       | (SUCC nat, ZERO) => false
       | (ZERO, SUCC nat) => true
       | (SUCC nat1, SUCC nat2) => less_than (nat1, nat2);



(****************** Next Section ******************)
datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

(* Problem 17 *)
(* Intersection will be incorrect for non-overlapping sets *)
fun isEmpty intSet =
    case intSet of
         Elems li => null li
       | Range {from, to} => from = to
       | Union (iSet1, iSet2) => (isEmpty iSet1) andalso (isEmpty iSet2)
       | Intersection (iSet1, iSet2) => (isEmpty iSet1) orelse (isEmpty iSet2)


(* Problem 18 *)
(* Same problem with intersect, union should be fine *)
fun contains intSet num =
    case intSet of
         Elems li => List.exists (fn x => x = num) li
       | Range {from, to} => (num >= from) orelse (num <= to)
       | Union (iSet1, iSet2) => (contains iSet1 num) 
                                  orelse (contains iSet2 num)
       | Intersection(iSet1, iSet2) => (contains iSet1 num) 
                                        orelse (contains iSet2 num)



