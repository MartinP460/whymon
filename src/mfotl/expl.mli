(*******************************************************************)
(*     This is part of Explanator2, it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Dmitriy Traytel (UCPH)                                         *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base
open Pred

module Part : sig

  type 'a t = Abs_part of ((Domain.t, Domain.comparator_witness) Coset.t * 'a) list

  val tabulate: Domain.t list -> (Domain.t -> 'a) -> 'a -> 'a t

end

module Proof : sig

  type sp =
    | STT of int
    | SPred of int * string * Term.t list
    | SNeg of vp
    | SOrL of sp
    | SOrR of sp
    | SAnd of sp * sp
    | SImpL of vp
    | SImpR of sp
    | SIffSS of sp * sp
    | SIffVV of vp * vp
    | SExists of string * Domain.t * sp
    | SForall of string * (sp Part.t)
    | SPrev of sp
    | SNext of sp
    | SOnce of int * sp
    | SEventually of int * sp
    | SHistorically of int * int * sp Fdeque.t
    | SHistoricallyOut of int
    | SAlways of int * int * sp Fdeque.t
    | SSince of sp * sp Fdeque.t
    | SUntil of sp * sp Fdeque.t
  and vp =
    | VFF of int
    | VPred of int * string * Term.t list
    | VNeg of sp
    | VOr of vp * vp
    | VAndL of vp
    | VAndR of vp
    | VImp of sp * vp
    | VIffSV of sp * vp
    | VIffVS of vp * sp
    | VExists of string * (vp Part.t)
    | VForall of string * Domain.t * vp
    | VPrev of vp
    | VPrev0
    | VPrevOutL of int
    | VPrevOutR of int
    | VNext of vp
    | VNextOutL of int
    | VNextOutR of int
    | VOnceOut of int
    | VOnce of int * int * vp Fdeque.t
    | VEventually of int * int * vp Fdeque.t
    | VHistorically of int * vp
    | VAlways of int * vp
    | VSinceOut of int
    | VSince of int * vp * vp Fdeque.t
    | VSinceInf of int * int * vp Fdeque.t
    | VUntil of int * vp * vp Fdeque.t
    | VUntilInf of int * int * vp Fdeque.t

  type t = S of sp | V of vp

  val s_append: sp -> sp -> sp
  val v_append: vp -> vp -> vp
  val s_drop: sp -> sp option
  val v_drop: vp -> vp option

  val s_at: sp -> int
  val v_at: vp -> int
  val p_at: t -> int

  val s_ltp: sp -> int
  val v_etp: vp -> int

  val s_to_string: string -> sp -> string
  val v_to_string: string -> vp -> string
  val to_string: t -> string

  module Size : sig

    val minp_bool: t -> t -> bool
    val minp: t -> t -> t
    val minp_list: t list -> t

  end

end

type 'a pdt = Leaf of 'a | Node of Term.t * ('a pdt) Part.t

type t = Proof.t pdt

val at: Proof.t pdt -> int

val apply1: Term.t list -> ('a -> 'b) -> 'a pdt -> 'b pdt

val apply2: Term.t list -> ('a -> 'b -> 'c) -> 'a pdt -> 'b pdt -> 'c pdt

val to_string: string -> t -> string
