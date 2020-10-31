
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
(* Write a function oldest that takes a list of dates and evaluates to an
* (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d
* if the date d is the oldest date in the list. *)
fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else
    let 
      fun oldest_nonempty(dates: (int*int*int) list) = 
        if null (tl dates)
        then SOME (hd dates)
        else 
              
      
      



