(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl(fn (a, b) => if (String.size a) <= (String.size b) then b else a) "";
val longest_string2 = List.foldl(fn (a, b) => if (String.size a) < (String.size b) then b else a) "";

fun longest_helper cmp acc string_list =
  case string_list of
      [] => acc
    | x::rest  => if cmp (String.size x, String.size acc)
		  then longest_helper cmp x rest
		  else longest_helper cmp acc rest

val longest_string3 = longest_helper (fn(a, b) => a > b) ""
val longest_string4 = longest_helper (fn(a, b) => a >= b) ""

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer func list =
  case list of
      [] => raise NoAnswer
    | x::rest => case func x of
		     NONE => first_answer func rest
		   | SOME v => v


fun all_answers f ls =
  let fun aux (f, ls, acc) =
	case ls of
	    [] => case acc of
							[] => NONE
						|	SOME ac => SOME ac
	  | head::tail => case f head of
			      NONE => aux(f, tail, acc)
			    | SOME v => aux(f, tail, acc@v)
  in
      aux(f, ls, [])
  end

fun count_wildcards p = g (fn(e) => 1) (fn(e) => 0) p

fun count_wild_and_variable_lengths p = ((g(fn(e) => 1) (fn(e) => 0) p) + (g(fn(e) => 0) (fn(s) => String.size s) p))

fun count_some_var (str, p) = g (fn(e) => 0) ( fn(e) => if str = e then 1 else 0) p

fun check_pat p =

  let fun generate_variable_string_list (p) =
	case p of
	    Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p, accu) => accu @ generate_variable_string_list(p)) [] ps
			      | ConstructorP (s, cp) => generate_variable_string_list(cp)
	  | _                 => []
      fun is_duplicate (string_list: string list) =
	case string_list of
            [] => false
          | s:: sl2 => List.exists (fn (e) => (s=e)) sl2 orelse is_duplicate (sl2)
  in
      not (is_duplicate(generate_variable_string_list(p)))
  end

fun match(valu, pattern) =
  case pattern of
      Wildcard => SOME []
    | UnitP =>
      (case valu of
	  Unit => SOME []
	| _ => NONE)
    | ConstP i =>
      (case valu of
	   Const v => if i = v then SOME [] else NONE
	 | _ => NONE)
    | Variable s => SOME [(s, valu)]
    | ConstructorP (s1, p) => (case valu of
				   Constructor (s2, v) => if (s1 = s2) then match (v, p) else NONE
				 | _ => NONE)
    | TupleP ps => case valu of
                       Tuple vs => if (List.length ps = List.length vs) then
				       all_answers match (ListPair.zip (vs, ps)) else NONE
                     | _ => NONE

fun first_match valu p_list =
  SOME (first_answer (fn (p) => match (valu, p)) p_list) handle NoAnswer => NONE
