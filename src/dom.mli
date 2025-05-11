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

type tt = TInt | TStr | TFloat | TENat [@@deriving compare, sexp_of, hash]

type enat = Nat of int | Inf [@@deriving compare, sexp_of, hash]

type t = Int of int | Str of string | Float of float | ENat of enat [@@deriving compare, sexp_of, hash]

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t

val equal: t -> t -> bool

val compare_t: t -> t -> int

val tt_equal: tt -> tt -> bool

val tt_of_string: string -> tt

val tt_of_domain: t -> tt

val tt_to_string: tt -> string

val tt_default: tt -> t

val enat_add: t * t -> t
val enat_mul: t * t -> t
val enat_sup: t * t -> t
val enat_min: t * t -> t

val string_to_t: string -> tt -> t

val to_string: t -> string

val list_to_string: t list -> string
