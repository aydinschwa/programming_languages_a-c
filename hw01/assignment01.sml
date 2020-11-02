
(* Assignment 1
* date -> int*int*int
* year, month, day
* positive year, 1<= month <= 12, 1 <= day <= 31 (depending on month) *)

(* Problem 1 *)
fun is_older(date1: int*int*int, date2: int*int*int) = 
  if (#1 date1) > (#1 date2)
  then true
  else if (#2 date1) > (#2 date2)
  then true
  else if (#3 date1) > (#3 date2)
  then true
  else false


(* Problem 2 *)
fun number_in_month(date_list: (int*int*int) list, month: int) =
  if null date_list
  then 0
  else if (#2 (hd(date_list))) = month
  then 1 + number_in_month(tl(date_list), month)
  else number_in_month(tl(date_list), month)


(* Problem 3 *)
fun number_in_months(date_list: (int*int*int) list, month_list: int list) = 
  if null(month_list)
  then 0
  else number_in_month(date_list,(hd month_list)) + number_in_months(date_list,(tl month_list))


(* Problem 4 *)
fun dates_in_month(date_list: (int*int*int) list, month: int) = 
  if null date_list
  then []
  else if (#2 (hd date_list) = month)
  then (hd date_list)::dates_in_month((tl date_list), month)
  else dates_in_month(tl date_list, month)


(* Problem 5 *)
fun dates_in_months(date_list: (int*int*int) list, month_list: int list) = 
  if null month_list
  then []
  else 
    if null date_list
    then []
    else dates_in_month(date_list, (hd month_list))@dates_in_months(date_list,(tl month_list))
    
    
(* Problem 6 *)
fun get_nth(strings: string list, index: int) = 
  if index = 0
  then hd strings
  else get_nth((tl strings), index-1)


(* Problem 7 *)
fun date_to_string(date: (int*int*int)) = 
  let 
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val months_list = ["January", "February", "March", "April", "May", "June", "July",
                         "August", "September", "October", "November", "December"]
    val ans = ""
  in
    get_nth(months_list, month-1)^" "^Int.toString(day)^", "^Int.toString(year)
  end


(* Problem 8 *)
fun number_before_reaching_sum(sum: int, num_list: int list) = 
  if sum - (hd num_list) <= 0
  then 0
  else (hd num_list) + number_before_reaching_sum(sum - (hd num_list), (tl
  num_list))


(* Problem 9 *)
fun what_month(day: int) = 
  let
    fun subtract_days(ans: int, days_per_month: int list) =
      if ans = 0
      then 1
      else 1 + subtract_days(ans - (hd days_per_month), (tl days_per_month))
    val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val ans = number_before_reaching_sum(day, days_per_month)
    in
      subtract_days(ans, days_per_month)
    end


(* Problem 10 *)
fun month_range(day1: int, day2: int) = 
  if day1 > day2
  then [] 
  else if day1 = day2
  then [what_month(day1)]
  else what_month(day1)::month_range(day1+1, day2)


(* Problem 11 *)
fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else let 
           fun oldest_nonempty(dates: (int*int*int) list) = 
             if null (tl dates)
             then hd dates
             else let val tl_ans = oldest_nonempty(tl dates)
                  in
                    if ((#1 tl_ans) < (#1 (hd dates)))
                    then tl_ans
                    else if (((#2 tl_ans) < (#2 (hd dates))) andalso ((#1 tl_ans)
                    = (#1 (hd dates))))
                    then tl_ans
                    else if (((#3 tl_ans) < (#3 (hd dates))) andalso ((#1 tl_ans)
                    = (#1 (hd dates)) andalso ((#2 tl_ans)
                    = (#2 (hd dates)))))
                    then tl_ans
                    else hd dates
                  end
        in 
           SOME (oldest_nonempty(dates))
        end
      
(* Problem 12 *)
fun remove_dups(month_list: int list) = 
  if null month_list
  then []
  else let fun check_dups(month_list: int list, month: int) = 
               if null month_list
               then false
               else if (hd month_list) = month
               then true
               else check_dups((tl month_list), month)
       in
         if check_dups(tl month_list, hd month_list)
         then remove_dups(tl month_list)
         else (hd month_list)::remove_dups(tl month_list)
       end


fun number_in_months_challenge(date_list: (int*int*int) list, month_list: int list) = 
  let val month_list = remove_dups(month_list)
  in
    if null(month_list)
    then 0
    else number_in_month(date_list,(hd month_list)) + number_in_months(date_list,(tl month_list))
  end 


fun dates_in_months_challenge(date_list: (int*int*int) list, month_list: int list) = 
  let val month_list = remove_dups(month_list)
  in 
    if null month_list
    then []
    else 
      if null date_list
      then []
      else dates_in_month(date_list, (hd month_list))@dates_in_months(date_list,(tl month_list))
  end


(* Problem 13 *)
(* Write a function reasonable_date that takes a date and determines if it
* describes a real date in the common era. A “real date” has a positive year
* (year 0 did not exist), a month between 1 and 12, and a day appropriate for
* the month. Solutions should properly handle leap years. Leap years are years
* that are either divisible by 400 or divisible by 4 but not divisible by 100.
* (Do not worry about days possibly lost in the conversion to the Gregorian
* calendar in the Late 1500s.) *)
fun days_in_month(month: int, month_list: int list) =
  if month-1 = 0
  then hd month_list
  else days_in_month(month-1, tl month_list)


fun reasonable_date(date: (int*int*int)) = 
  let 
    val day = #3 date
    val month = #2 date
    val year = #1 date
    val no_leap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val leap =  [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 
    if year <= 0
    then false
    else if month < 1 orelse month > 12
    then false
    else if day < 0
    then false
    (* leap year case *)
    else if (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
    then
      if day > days_in_month(month, leap)
      then false
      else true
    else if day > days_in_month(month, no_leap)
    then false
    else true
  end




