(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2
fun all_except_option(str: string, dict : string list) =
  case dict of
      [] => NONE
    | now :: next => let
	              val rest = all_except_option(str, next)
    in
	case rest of
	    NONE => if (same_string(now, str)) then SOME next else NONE
	  | SOME next => if (same_string(now, str)) then SOME next else SOME (now :: next)
    end
fun get_substitutions1(dict : string list list, str : string) =
  case dict of
      [] => []
    | now :: next =>
      let
	  val newlist = all_except_option(str, now)
      in
	  case newlist of
	      NONE => get_substitutions1(next, str)
	   | SOME result => result @ get_substitutions1(next, str) 
      end
	  
fun get_substitutions2(dict : string list list, str : string) =
  let
      fun helper(dict : string list list, str : string, acc : string list) =
	case dict of
	    [] => acc
	  | now :: next =>
    	    let
		val newlist = all_except_option(str, now)
	    in
		case newlist of
		    	      NONE => helper(next, str, acc)
			    | SOME result => helper(next, str, acc @ result)
	    end
  in
      helper(dict, str, [])
  end
      
fun similar_names(dict : string list list,  {first = x, middle = y, last = z}) =
  let
      val subs = get_substitutions2(dict, x)
      fun helper(dict : string list, acc : {first : string, middle : string, last : string} list) =
	case dict of
	    [] => acc
	  | now :: next => helper(next, acc @ [{first = now, middle = y, last = z}])
  in
      helper(x :: subs, [])
  end
      

				
	
	
	
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(now : card) =
  case now of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (_, _) => Red

fun card_value(now : card) =
  case now of
      (_, Num i) => i
    | (_, Ace) => 11
    | (_, _) => 10

fun remove_card(cs : card list, c : card, ex) =
  case cs of
      [] => raise ex
    | (now :: next) =>
      if (now = c) then next else now :: remove_card(next, c, ex)
						    
fun all_same_color(cs : card list) =
  case cs of
      [] => false
    | (_::[]) => true
    | (c1::(c2::next)) => if (card_color(c1) = card_color(c2)) then all_same_color(c2::next) else false

fun sum_cards(cs : card list) =
  let
      fun helper(cs : card list, acc : int) =
	case cs of
	    [] => acc
	  | (now :: next) => helper(next, card_value(now) + acc)
  in
      helper(cs, 0)
  end
      
fun score(cs : card list, goal : int) =
  let fun prelim(cs : card list, goal : int) = 
      let
	  val now =  sum_cards(cs)				   
      in	  
	  if (now >= goal) then 3 * (now - goal)					  
	  else goal - now			  
      end	  
  in      
    if (all_same_color(cs)) then prelim(cs, goal) div 2
    else prelim(cs, goal)
  end

fun officiate(cs : card list, ms : move list, goal : int) =
  let
      fun run(cs : card list, ms : move list, hs : card list) =
	case ms of
	    [] => score(hs, goal)
	  | (Discard c :: nextms) => run(cs, nextms, remove_card(hs, c, IllegalMove))
	  | (Draw :: nextms) =>
	    case cs of
		[] => score(hs, goal)
	      | card :: nextcs =>
		let
		    val newhs = card :: hs
		in
		    if (sum_cards(hs) > goal) then score(hs, goal)
		    else run(nextcs, nextms, newhs)
		end		    
  in
      run(cs, ms, [])
  end
      
      
	       
      
