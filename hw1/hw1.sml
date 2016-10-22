fun is_older(DateA : int * int * int, DateB : int * int * int) =
  if (#1 DateA <> #1 DateB)
  then (#1 DateA < #1 DateB)
  else
      if (#2 DateA <> #2 DateB)
      then (#2 DateA < #2 DateB)
      else (#3 DateA < #3 DateB)

fun number_in_month(DateList : (int * int * int) list, Month : int) =
  if null DateList
  then 0
  else
      let val next = number_in_month(tl DateList, Month)
      in
	  if #2 (hd DateList) = Month
	  then 1 + next
	  else next
      end

fun number_in_months(DateList : (int * int * int) list, MonthList : int list) =
  if null MonthList
  then 0
  else
      number_in_month(DateList, hd MonthList) + number_in_months(DateList, tl MonthList)
								
fun dates_in_month(DateList : (int * int * int) list, Month : int) =
  if null DateList
  then []
  else
      let val next = dates_in_month(tl DateList, Month)
      in
	  if #2 (hd DateList) = Month
	  then (hd DateList) :: next
	  else next
      end
		   
fun dates_in_months(DateList : (int * int * int) list, MonthList : int list) =
  if null MonthList
  then []
  else
      dates_in_month(DateList, hd MonthList) @ dates_in_months(DateList, tl MonthList)

fun get_nth(StringList : string list, nth : int) =
  if (nth = 1)
  then hd StringList
  else get_nth(tl StringList, nth - 1)
	      
val Months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string(Date : (int * int * int)) =
  get_nth(Months, #2 Date) ^ " " ^ Int.toString(#3 Date) ^ ", " ^ Int.toString(#1 Date)

fun number_before_reaching_sum(Sum : int, Numbers : int list) =
  if null Numbers
  then 0
  else
      if (hd Numbers) < Sum
      then 1 + number_before_reaching_sum(Sum -(hd Numbers), tl Numbers)
      else 0
	       
val Dates = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

fun what_month(Date : int) =
  number_before_reaching_sum(Date, Dates) + 1
						
fun month_range(Date1 : int, Date2 : int) =
  if (Date1 > Date2)
  then []
  else
      what_month(Date1) :: month_range(Date1 + 1, Date2)
			       
fun oldest(DateList : (int * int * int) list) =
  if null DateList
  then NONE
  else
      let val told = oldest(tl DateList)
      in
	  if (isSome told andalso is_older(valOf told, hd DateList))
	  then told
	  else SOME (hd DateList)
      end
	  
