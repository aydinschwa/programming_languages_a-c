

(* Problem 1 *)
fun alternate num_list =
    case num_list of
         []               => 0
       | num1::num2::rest => num1 - num2 + alternate rest

(* Problem 2 *)
fun min_max num_list =
    let fun aux num_list min max =
        case num_list of
            []        => (min, max)
          | num::rest => if num < min
                         then aux rest num max
                         else if num > max
                         then aux rest min num
                         else aux rest min max

    in
        aux num_list 99999 ~99999
    end


(* Problem 3 *)
fun cumsum num_list =
    List.foldl (fn (x, y) => x + y) 0 num_list

(* Problem 4 *)
fun greeting str_opt =
    case str_opt of
         NONE      => "Hello there, you!"
       | SOME name => "Hello there, "^name^"!"

(* Problem 5 *)
exception ListMismatch
fun repeat nums1 nums2 =
    let 
        fun ptl_list num1 num2 =
            if num2 > 0
            then num1::(ptl_list num1 (num2-1))
            else []
    in
        case (nums1, nums2) of
             ([], [])               => []
           | (num1::tl1, num2::tl2) => (ptl_list num1 num2)@(repeat tl1 tl2)
           | (_, _)                 => raise ListMismatch
    end


(* Problem 6 *)
fun addOpt optint1 optint2 =
    case (optint1, optint2) of
         (NONE, _) => NONE
       | (_, NONE) => NONE
       | (SOME i1, SOME i2) => SOME (i1 + i2)


(* Problem 7 *)
fun addAllOpt optintlist =
    let fun aux optintlist acc =
            case optintlist of
                 [] => acc
               | NONE::tl => aux tl acc
               | (SOME i)::tl => aux tl (acc+i)
    in
        aux optintlist 0
    end


(* Problem 8 *)
fun any tf_list =
    case tf_list of
         [] => false
       | tf::tl => tf orelse any tl


(* Problem 9 *)
fun all tf_list =
    case tf_list of
         [] => true 
       | tf::tl => tf andalso any tl


(* Problem 10 *)
fun zip il1 il2 =
    case (il1, il2) of
         ([], []) => []
       | (v1::tl1, v2::tl2) => (v1, v2)::(zip tl1 tl2)
       | (_, _) => []

(* Problem 11 *)
exception Wrong
fun sublist li num =
    case (li, num) of
        (_, 0) => []
      | (elem::tl, num) => elem::(sublist tl (num-1))
      | (_, _) => raise Wrong

fun zipRecycle il1 il2 =
    let
        val diff = List.length il1 - List.length il2
        fun elongate li num =
            if num >= (List.length li)
            then elongate (li@li) (num - List.length li)
            else li@(sublist li num)
    in
        if diff < 0
        then zip (elongate il1 (~diff)) il2
        else zip il1 (elongate il2 diff) 
    end


(* Problem 12 *)
fun zipOpt il1 il2 =
    let fun zipmod il1 il2 =
            case (il1, il2) of
                 ([], []) => []
               | (v1::tl1, v2::tl2) => (v1, v2)::(zipmod tl1 tl2)
               | (_, _) => raise ListMismatch 
    in 
        SOME (zipmod il1 il2) handle ListMismatch => NONE
    end


(* Problem 13 *)
fun lookup str_int_list str =
    case str_int_list of
         [] => NONE
       | (str1, num)::tl => if str1 = str then SOME num else lookup tl str


(* Problem 14 *)
fun splitup ints =
    let
        fun aux ints pos neg =
            case ints of
                 [] => (List.rev pos, List.rev neg)
               | num::tl => if num >= 0 
                            then aux tl (num::pos) neg
                            else aux tl pos (num::neg)
    in
        aux ints [] []
    end


(* Problem 15 *)
fun splitAt num_list cut =
    let
        fun aux num_list pos neg =
            case num_list of
                 [] => (List.rev pos, List.rev neg)
               | num::tl => if num >= cut
                            then aux tl (num::pos) neg
                            else aux tl pos (num::neg)
    in
        aux num_list [] [] 
    end


(* Problem 16 *)
fun isSorted ints =
    case ints of
         num1::num2::tl => num1 <= num2 andalso isSorted (num2::tl)
       | _ => true


(* Problem 17 *)
fun isAnySorted ints =
    let fun aux ints f = 
        case ints of
            num1::num2::tl => true andalso aux (num2::tl)
          | num::tl => true
          | [] => true
    in
        aux ints (fn (x,y) => x >= y)
    end



