(*******************************************************************)
(*     This is part of WhyMon, and it is distributed under the     *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Dmitriy Traytel (UCPH)                                         *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base

module T = struct

  type tt = TInt | TStr | TFloat | TENat | TRange [@@deriving compare, sexp_of, hash]

  type enat = Nat of int | Inf [@@deriving compare, sexp_of, hash]

  type range = enat * enat [@@deriving compare, sexp_of, hash]

  type t = Int of int | Str of string | Float of float | ENat of enat | Range of range [@@deriving compare, sexp_of, hash]

  let rec equal d d' = match d, d' with
    | Int v, Int v' -> Int.equal v v'
    | Str v, Str v' -> String.equal v v'
    | Float v, Float v' -> Float.equal v v'
    | ENat v, ENat v' ->
      (match v, v' with
      | Nat n, Nat n' -> Int.equal n n'
      | Inf, Inf -> true
      | _ -> false)
    (* TODO: Add cases for comparing ranges with enats and ints. *)
    | Range (l, u), Range (l', u') -> 
      equal (ENat l) (ENat l') && equal (ENat u) (ENat u')
    | _ -> false

  let compare_t d d' = match d, d' with
    | Int v, Int v' -> if Int.equal v v' then 0
                       else (if Int.(>) v v' then 1 else -1)
    | Str v, Str v' -> if String.equal v v' then 0
                       else (if String.(>) v v' then 1 else -1)
    | Float v, Float v' -> if Float.equal v v' then 0
                           else (if Float.(>) v v' then 1 else -1)
    | ENat v, ENat v' -> (match v, v' with
                           | Nat i, Nat i' -> if Int.equal i i' then 0 
                              else (if Int.(>) i i' then 1 else -1)
                           | Inf, Inf -> 0
                           | Nat _, Inf -> -1
                           | Inf, Nat _ -> 1)
    | Range v, Range v' -> (match v, v with
                           (* Case of ranges (0, n) and (0, n). Two less than operators. *)
                           | (Nat l, Nat u), (Nat l', Nat u') ->
                              if Int.equal u u' then 0
                              else (if Int.(>) u u' then 1 else -1)
                           (* Case of ranges (n, INF) and (n, INF). Two greater than operators. *)
                           | (Nat l, Inf), (Nat l', Inf) ->
                              if Int.equal l l' then 0
                              else (if Int.(>) l l' then 1 else -1)
                           (* Case of ranges (0, n) and (n, INF) and vice versa. One less than, one greater than operator. *)
                           | (Nat _, Nat _), (Nat _, Inf) -> -1
                           | (Nat _, Inf), (Nat _, Nat _) -> 1
                           (* TODO: Missing case for (INF, INF) and (0, INF). *)
                           | _ -> raise (Invalid_argument "Range comparison not supported for other cases"))
    | Int _, Str _ -> 1
    | Int _, Float _ -> 1
    | Int _, ENat _ -> 1
    | Int _, Range _ -> 1
    | Str _, Int _ -> -1
    | Str _, Float _ -> 1
    | Str _, ENat _ -> 1
    | Str _, Range _ -> 1
    | Float _, Int _ -> -1
    | Float _, Str _ -> -1
    | Float _, ENat _ -> 1
    | Float _, Range _ -> 1
    | ENat _, Int _ -> -1
    | ENat _, Str _ -> -1
    | ENat _, Float _ -> -1
    | ENat _, Range _ -> 1
    | Range _, Int _ -> -1
    | Range _, Str _ -> -1
    | Range _, Float _ -> -1
    | Range _, ENat _ -> -1
  let tt_equal tt tt' = match tt, tt' with
    | TInt, TInt
      | TStr, TStr
      | TFloat, TFloat
      | TENat, TENat
      | TRange, TRange -> true
    | _ -> false

  let tt_of_string = function
    | "int" -> TInt
    | "string" -> TStr
    | "float" -> TFloat
    | "enat" -> TENat
    | t -> raise (Invalid_argument (Printf.sprintf "type %s is not supported" t))

  let tt_of_domain = function
    | Int _ -> TInt
    | Str _ -> TStr
    | Float _ -> TFloat
    | ENat _ -> TENat
    | Range _ -> TRange

  let tt_to_string = function
    | TInt -> "int"
    | TStr -> "string"
    | TFloat -> "float"
    | TENat -> "enat"
    | TRange -> "range"

  let tt_default = function
    | TInt -> Int 0
    | TStr -> Str ""
    | TFloat -> Float 0.0
    | TENat -> ENat (Nat 0)
    | TRange -> Range (Nat 0, Nat 0)

  let string_to_t s tt = match tt with
    | TInt -> (try Int (Int.of_string s)
               with Failure _ -> raise (Invalid_argument (Printf.sprintf "%s is not an int" s)))
    | TStr -> Str s
    | TFloat -> (try Float (Float.of_string s)
                 with Failure _ -> raise (Invalid_argument (Printf.sprintf "%s is not a float" s)))
    | TENat -> ENat (if String.equal s "INF" then Inf else (
      let s_int = (Int.of_string s) in
      if s_int >= 0 then Nat s_int else raise (Invalid_argument (Printf.sprintf "%s is not an extended natural number" s))
     ))
    | TRange -> raise (Invalid_argument "Range cannot explicitly be used as a type")
  
  let to_enat = function
    | ENat (Nat n) -> Nat n
    | Int n -> if n >= 0 then Nat n else raise (Invalid_argument "%s is not a natural number")
    | _ -> raise (Invalid_argument "Value not supported for extended natural numbers")

  let unENat = function
    | ENat en -> en
    | _ -> raise (Invalid_argument "unENat is not defined for non-natural numbers")

  let enat_add = function
    | ENat (Nat n), ENat (Nat n') -> ENat (Nat (n + n'))
    | ENat _, ENat _ -> ENat Inf
    | _ -> raise (Invalid_argument "Extended natural number addition not defined for non-natural numbers")

  let enat_sub = function
    | ENat (Nat n), ENat (Nat n') -> ENat (Nat (n - n'))
    | ENat Inf, ENat Inf -> ENat (Nat 0)
    | ENat _, ENat (Nat _) -> ENat Inf
    | ENat _, ENat Inf -> raise (Invalid_argument "Extended natural number subtraction resulted in negative infinity")
    | _ -> raise (Invalid_argument "Extended natural number subtraction not defined for non-natural numbers")

  let enat_mul = function
    | ENat (Nat n), ENat (Nat n') -> ENat (Nat (n * n'))
    | ENat (Nat 0), ENat Inf
    | ENat Inf, ENat (Nat 0) -> ENat (Nat 0)
    | ENat _, ENat _ -> ENat Inf
    | _ -> raise (Invalid_argument "Extended natural number multiplication not defined for non-natural numbers")

  let enat_sup = function
    | ENat (Nat n), ENat (Nat n') -> ENat (Nat (max n n'))
    | ENat _, ENat _ -> ENat Inf
    | Str s, ENat _
    | ENat _, Str s -> Str s
    | Str s, Str s' -> Str (String.max s s')
    | _ -> raise (Invalid_argument "Extended natural number sup not defined for non-natural numbers or strings")

  let enat_min = function
    | ENat (Nat n), ENat (Nat n') -> ENat (Nat (min n n'))
    | ENat (Nat n), ENat _
    | ENat _, ENat (Nat n) -> ENat (Nat n)
    | ENat _, ENat _ -> ENat Inf
    | Str s, ENat _
    | ENat _, Str s -> Str s
    | Str s, Str s' -> Str (String.min s s')
    | _ -> raise (Invalid_argument "Extended natural number min not defined for non-natural numbers or strings")

  let rec to_string = function
    | Int v -> Int.to_string v
    | Str v -> v
    | Float v -> Float.to_string v
    | ENat v -> (match v with Nat i -> Int.to_string i | _ -> "INF")
    | Range (l, v) -> Printf.sprintf "%s..%s" (to_string (ENat l)) (to_string (ENat v))

  let list_to_string ds =
    String.drop_suffix (List.fold ds ~init:"" ~f:(fun acc d -> acc ^ (to_string d) ^ ", ")) 2

end

include T
include Comparator.Make(T)
