(* Problem 1 *)
fun compose_opt f g x =
    case (g x) of
         NONE => NONE
       | SOME v => f (SOME v) 
        

(* Problem 2 *)
fun do_until f1 f2 x =
    case f2 x of
         true  => do_until f1 f2 (f1 x)
       | false => x


(* Problem 3 *)
fun fact num =
  #2 (do_until (fn (num, acc) => ((num-1), acc*num)) 
             (fn (num, acc) => (num > 0)) (num, 1))


(* Problem 4 *)
fun fixed_point f x =
    do_until (fn pt => f pt) (fn pt => pt <> x) x


(* Problem 5 *)
fun map2 f (p1, p2) =
    (f p1, f p2)

(* Problem 6 *)
fun app_all f g x =
    let 
        val init = g x
        fun aux f init =
            case init of
                 [] => []
               | el::tl => (f el)@(aux f tl)

    in
        aux f init
    end


(* Problem 7 *)
fun foldl f acc li =
    case li of
         [] => acc
       | hd::tl => foldl f (f acc hd) tl

fun foldr f acc li =
    foldl f acc (List.rev li)


(* Problem 8 *)
fun partition f li =
    let fun aux f li tr fl =
            case li of
                 [] => (tr, fl)
               | el::tl => if f el 
                           then aux f tl (el::tr) fl
                           else aux f tl tr (el::fl)
    in
        aux f li [] []
    end



