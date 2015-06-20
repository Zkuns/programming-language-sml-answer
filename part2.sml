(* This is comment *)
(*val a = 12;

val b = 24;

val c = 23;

val a = 4;

val d = a;
*)(*  *)
(*
fun hi(x : int, y : string) =
  if x = 2
  then "hi"
  else y

val r = hi(1, "zhukun");

fun pow(x : int, y : int) =
  if x = 2
  then 3
  else y

val result = pow(2, 3)*)

(*fun t(x : (int * int), y : (int * bool)) =
  if (#2 y)
  then (#1 x) * (#2 x)
  else (#1 y)

val a = t((2,3), (7,true))*)

(*
fun myfun(z : int list) =
  if null z
  then 1
  else hd z * myfun(tl z)

fun mybefore(x : int, y : int)=
  if x=y
  then []
  else y :: mybefore(x, y-1)

fun countup(x : int, y : int)=
  if x=y
  then [y]
  else x::countup(x+1,y)
fun countdown(x : int, y : int)=
  if x=y
  then [y]
  else x::countdown(x-1, y)

fun max(z : int list)=
  if null z
  then 0
  else if null(tl z)
  then hd z
  else
    let val maxnum = max(tl z)
    in
      if hd z > maxnum
      then hd z
      else maxnum
    end

fun omax(z : int list)=
  if null z
  then NONE
  else if null(tl z)
  then SOME (hd z)
  else
    let val maxnum = omax(tl z)
    in if isSome maxnum andalso valOf maxnum > hd z
       then maxnum
       else SOME (hd z)
    end
*)

(*  1  *)
fun is_older(date1:int*int*int, date2:int*int*int) =
  if #1 date1 <> #1 date2
  then #1 date1 < #1 date2
  else if #2 date1 <> #2 date2
  then #2 date1 < #2 date2
  else if #3 date1 <> #3 date2
  then #3 date1 < #3 date2
  else false
(*what the f*)
(*
fun is_older(date1:int*int*int, date2:int*int*int) =
  (#1 date1 < #1 date2) orelse #2 date1 < #2 date2 orelse #3 date1 < #3 date2
*)


(* 2 *)
fun number_in_month(dates: (int*int*int) list, x:int) =
  if null dates
  then 0
  else if #2 (hd dates) = x
  then number_in_month(tl dates, x) + 1
  else number_in_month(tl dates, x)


(* 3 *)
fun number_in_months(dates:(int*int*int) list, months:int list)=
  if null(tl months)
  then number_in_month(dates, hd months)
  else number_in_month(dates, hd months)+number_in_months(dates, tl months)

fun number_in_months(dates:(int*int*int) list, months:int list)=
  if null months
  then NONE
  else if null(tl months)
  then SOME(number_in_month(dates, hd months))
  else SOME(number_in_month(dates, hd months) + valOf(number_in_months(dates, tl months)))


(* 4 *)
fun dates_in_month(dates:(int*int*int) list, month: int)=
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

(* 5 *)
fun dates_in_months(dates:(int*int*int) list, months:int list)=
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 *)
fun get_nth(strs: string list, n: int )=
  if n=1
  then hd strs
  else get_nth(tl strs, n - 1)

(* 7 *)
(* January, February, March, April, May, June, July, August, September, October,
* November, December *)
fun date_to_string(date: (int*int*int))=
  let
    val pair = ["January", "February", "March", "April", "May", "June", "August", "September", "October", "November", "December"]
    val str = get_nth (pair,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  in
    str
  end

fun zreturn(temp: int, znumbers: int list, sum: int)=
      (*temp = temp + hd znumbers*)
      if (temp < sum) andalso (not (null (tl znumbers)))
      then 1 + zreturn(temp, tl znumbers, sum)
      else if temp < sum
      then 1
      else 1

(* 8 *)
fun number_before_reaching_sum(sum: int, numbers: int list)=
  let
    fun zreturn(temp: int, znumbers: int list, sum: int)=
      let
        val temp = temp + hd znumbers
      in
        if (temp < sum) andalso (not (null (tl znumbers)))
        then 1 + zreturn(temp, tl znumbers, sum)
        else if temp < sum
        then 1
        else 0
      end
  in
    if null numbers
    then 0
    else zreturn(0, numbers, sum)
  end
(* it is a stupid answer*)
fun number_before_reaching_sum(sum: int, numbers: int list)=
  if null numbers orelse sum < hd numbers
  then 1
  else number_before_reaching_sum(sum - hd numbers, tl numbers)+1

(* 9 *)
fun what_month(day: int)=
  let
    val month = [31, 30, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, month)
  end

(* 10 *)
fun month_range(day1: int, day2:int)=
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
fun oldest(dates: (int*int*int) list)=
  if null dates
  then NONE
  else if null(tl dates)
  then SOME (hd dates)
  else
    let
      val temp = oldest(tl dates)
    in
      if is_older(valOf temp, hd dates)
      then temp
      else SOME(hd dates)
    end

(*
fun test(dates: (int*int*int) list)=
  val a = SOME(hd dates)
  if is_older(val a, hd(tl dates))
  then a
  else SOME hd(tl dates)*)




datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza

fun f (x : mytype) =
        case x of
             TwoInts(i1,i2) => i1+i2
           | Str s => String.size s
           | Pizza => 1

datatype exec = Constant of int
              | Add      of exec * exec
              | Negate   of exec
              | Multiply of exec * exec

fun calculate e =
  case e of
       Constant(i)      => i
     | Add(e1, e2)      => (calculate(e1)) + (calculate(e2))
     | Negate(n)        => ~(calculate(n))
     | Multiply(e1, e2) => (calculate(e1)) * (calculate(e2))
