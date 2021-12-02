(*******************************************************************)
(*     This is part of Explanator2, it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2021:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Io
open Mtl
open Expl
open Util
open Interval
open Checker_interface

module Deque = Core_kernel.Deque
module List = Core_kernel.List

exception UNBOUNDED_FUTURE
exception INVALID_EXPL of string
exception EMPTY_DEQUE of string
exception EMPTY_LIST of string
exception NOT_FOUND of string

(* TODO: Rewrite every occurrence of Deque.to_list in this file *)
(* TODO: Rename every (ts, p) as el or whatever. p should denote the proof element and not the pair *)

let sappend_to_deque sp1 d =
  let () = Deque.iteri d ~f:(fun i (ts, ssp) ->
              match ssp with
              | S sp -> Deque.set_exn d i (ts, S (sappend sp sp1))
              | V _ -> raise SEXPL) in d

let vappend_to_deque vp2 d =
  let () = Deque.iteri d ~f:(fun i (ts, vvp) ->
              match vvp with
              | V vp -> Deque.set_exn d i (ts, V (vappend vp vp2))
              | S _ -> raise VEXPL) in d

let betas_suffix_in_to_list betas_suffix_in =
  Deque.fold' betas_suffix_in ~init:[]
    ~f:(fun acc (ts, vp) -> vp::acc) `back_to_front

let remove_if_pred_front f d =
  let rec aux f d =
    let el = Deque.dequeue_front d in
    match el with
    | None -> ()
    | Some(el') -> if (f el') then aux f d
                   else Deque.enqueue_front d el' in
  let () = aux f d in d

let remove_if_pred_front_ne f d =
  let rec aux f d =
    if (Deque.length d) > 1 then
      let el = Deque.dequeue_front d in
      match el with
      | None -> ()
      | Some(el') -> if (f el') then aux f d
                     else Deque.enqueue_front d el' in
  let () = aux f d in d

let remove_if_pred_back f d =
  let rec aux f d =
    let el = Deque.dequeue_back d in
    match el with
    | None -> ()
    | Some(el') -> if (f el') then aux f d
                   else Deque.enqueue_back d el' in
  let () = aux f d in d

let sorted_append new_in d le =
  let () = Deque.iter new_in ~f:(fun (ts, p) ->
               let _ = remove_if_pred_back (fun (ts', p') -> le p p') d in
               Deque.enqueue_back d (ts, p)) in
  d

let sorted_enqueue (ets, lts, p) d le =
  let _ = remove_if_pred_back (fun (ets', lts', p') -> le p p') d in
  let () = Deque.enqueue_back d (ets, lts, p) in d

(* TODO: split_in_out and split_out_in should be rewritten as a single function *)
(* Considering a closed interval [l, r] *)
let split_in_out get_ts (l, r) d =
  let new_in = Deque.create () in
  let rec aux d =
    let el_opt = Deque.dequeue_front d in
    match el_opt with
    | None -> ()
    | Some(el) -> (let ts = get_ts el in
                   if ts >= l && ts <= r then
                     (let () = Deque.enqueue_back new_in el in aux d)
                   else Deque.enqueue_front d el) in
  let () = aux d in
  (d, new_in)

(* Considering an interval of the form [z, l) *)
let split_out_in get_ts (z, l) d =
  let new_out = Deque.create () in
  let rec aux d =
    let el_opt = Deque.dequeue_front d in
    match el_opt with
    | None -> ()
    | Some(el) -> (let ts = get_ts el in
                   if ts >= z && ts < l then
                     (let () = Deque.enqueue_back new_out el in aux d)
                   else Deque.enqueue_front d el) in
  let () = aux d in
  (d, new_out)

(* TODO: Those functions are only used in the until case, replace them with
   the tail-recursive function mentioned above *)
let sort_ps le new_in =
  let rec aux ps acc =
    match ps with
    | [] -> acc
    | x::x'::xs ->
       if le (snd(x)) (snd(x')) then
         aux xs (x::acc)
       else aux xs (x'::acc)
    | x::xs -> x::acc
  in aux new_in []

module Past = struct
  type msaux = {
      ts_zero: timestamp option
    ; ts_tp_in: (timestamp * timepoint) Deque.t
    ; ts_tp_out: (timestamp * timepoint) Deque.t

    (* sorted deque of S^+ beta [alphas] *)
    ; beta_alphas: (timestamp * expl) Deque.t
    (* deque of S^+ beta [alphas] outside of the interval *)
    ; beta_alphas_out: (timestamp * expl) Deque.t

    (* sorted deque of S^- alpha [betas] *)
    ; alpha_betas: (timestamp * expl) Deque.t
    (* sorted deque of alpha proofs *)
    ; alphas_out: (timestamp * expl) Deque.t
    (* list of beta violations inside the interval *)
    ; betas_suffix_in: (timestamp * vexpl) Deque.t
    (* list of alpha/beta violations *)
    ; alphas_betas_out: (timestamp * vexpl option * vexpl option) Deque.t
    ; }

  let print_ts_lists { ts_zero; ts_tp_in; ts_tp_out } =
    Printf.fprintf stdout "%s" (
    (match ts_zero with
     | None -> ""
     | Some(ts) -> Printf.sprintf "\n\tts_zero = (%d)\n" ts) ^
    Deque.fold ts_tp_in ~init:"\n\tts_in = ["
      ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d, %d);" ts tp)) ^
      (Printf.sprintf "]\n") ^
    Deque.fold ts_tp_out ~init:"\n\tts_out = ["
      ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d, %d);" ts tp)) ^
      (Printf.sprintf "]\n"))

  let msaux_to_string { ts_zero
                      ; ts_tp_in
                      ; ts_tp_out
                      ; beta_alphas
                      ; beta_alphas_out
                      ; alpha_betas
                      ; alphas_out
                      ; betas_suffix_in
                      ; alphas_betas_out } =
    "\n\nmsaux: " ^
      (match ts_zero with
       | None -> ""
       | Some(ts) -> Printf.sprintf "\nts_zero = (%d)\n" ts) ^
      Deque.fold ts_tp_in ~init:"\nts_in = ["
        ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d, %d);" ts tp)) ^
      (Printf.sprintf "]\n") ^
      Deque.fold ts_tp_out ~init:"\nts_out = ["
        ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d, %d);" ts tp)) ^
      (Printf.sprintf "]\n") ^
      Deque.fold beta_alphas ~init:"\nbeta_alphas = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold beta_alphas_out ~init:"\nbeta_alphas_out = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold alpha_betas ~init:"\nalpha_betas = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold alphas_out ~init:"\nalphas_out = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold betas_suffix_in ~init:"\nbetas_suffix_in = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.v_to_string "" ps) ^
      Deque.fold alphas_betas_out ~init:"\nalphas_betas_out = "
        ~f:(fun acc (ts, p1_opt, p2_opt) ->
          match p1_opt, p2_opt with
          | None, None -> acc
          | Some(p1), None -> acc ^ (Printf.sprintf "\n(%d)\nalpha = " ts) ^
                              Expl.v_to_string "" p1
          | None, Some(p2) -> acc ^ (Printf.sprintf "\n(%d)\nbeta = " ts) ^
                              Expl.v_to_string "" p2
          | Some(p1), Some(p2) -> acc ^ (Printf.sprintf "\n(%d)\nalpha = " ts) ^
                                  Expl.v_to_string "" p1 ^
                                  (Printf.sprintf "\n(%d)\nbeta = " ts) ^
                                  Expl.v_to_string "" p2)

  let update_ts (l, r) a ts tp msaux =
    if a = 0 then
      let () = Deque.enqueue_back msaux.ts_tp_in (ts, tp) in
      let ts_tp_in = remove_if_pred_front (fun (ts', _) -> ts' < l) msaux.ts_tp_in in
      { msaux with ts_tp_in }
    else
      let () = Deque.enqueue_back msaux.ts_tp_out (ts, tp) in
      let () = Deque.iter msaux.ts_tp_out
                ~f:(fun (ts', tp') ->
                  if ts' <= r then
                    Deque.enqueue_back msaux.ts_tp_in (ts', tp')) in
      let ts_tp_out = remove_if_pred_front (fun (ts', _) -> ts' <= r) msaux.ts_tp_out in
      let ts_tp_in = remove_if_pred_front (fun (ts', _) -> ts' < l) msaux.ts_tp_in in
      { msaux with ts_tp_out; ts_tp_in }

  let add_alphas_out ts vvp' alphas_out le =
    let () = List.iter (List.rev(Deque.to_list alphas_out))
              ~f:(fun (_, vvp) ->
                if le vvp' vvp then Deque.drop_back alphas_out) in
    let () = Deque.enqueue_back alphas_out (ts, vvp')
    in alphas_out

  let update_beta_alphas new_in beta_alphas le =
    if (Deque.is_empty new_in) then beta_alphas
    else sorted_append new_in beta_alphas le

  let update_betas_suffix_in new_in betas_suffix_in =
    let _ = List.iter new_in
              ~f:(fun (ts, _, vp2_opt) ->
                match vp2_opt with
                | None -> Deque.clear betas_suffix_in
                | Some(vp2) -> Deque.enqueue_back betas_suffix_in (ts, vp2))
    in betas_suffix_in

  let construct_vsinceps tp new_in =
    Deque.fold new_in ~init:(Deque.create ())
      ~f:(fun acc (ts, vp1_opt, vp2_opt) ->
        match vp1_opt with
        | None ->
           (match vp2_opt with
            | None -> (let () = Deque.clear acc in acc)
            | Some(vp2) -> (let new_acc = vappend_to_deque vp2 acc in new_acc))
        | Some(vp1) ->
           (match vp2_opt with
            | None -> (let () = Deque.clear acc in acc)
            | Some(vp2) -> (let new_acc = vappend_to_deque vp2 acc in
                            let vp = V (VSince (tp, vp1, [vp2])) in
                            let () = Deque.enqueue_back new_acc (ts, vp) in
                            new_acc)))

  let add_new_ps_alpha_betas tp new_in alpha_betas le =
    let new_vps_in = construct_vsinceps tp new_in in
    if not (Deque.is_empty new_vps_in) then
      sorted_append new_vps_in alpha_betas le
    else alpha_betas

  let update_alpha_betas_tps tp alpha_betas =
    let () = Deque.iteri alpha_betas ~f:(fun i (ts, vvp) ->
                 match vvp with
                 | V (VSince (tp', vp1, vp2s)) -> Deque.set_exn alpha_betas i (ts, V (VSince (tp, vp1, vp2s)))
                 | _ -> raise (INVALID_EXPL "Explanation should be VSince")) in
    alpha_betas

  let update_alpha_betas tp new_in alpha_betas le =
    let alpha_betas_vapp = Deque.fold new_in ~init:alpha_betas
                             ~f:(fun d (_, _, vp2_opt) ->
                               match vp2_opt with
                               | None -> let _ = Deque.clear alpha_betas in d
                               | Some(vp2) -> vappend_to_deque vp2 d) in
    let alpha_betas' = add_new_ps_alpha_betas tp new_in alpha_betas_vapp le in
    (update_alpha_betas_tps tp alpha_betas')

  let etp ts_tp_in ts_tp_out tp =
    match Deque.peek_front ts_tp_in with
    | None -> (match Deque.peek_front ts_tp_out with
               | None -> tp
               | Some (_, tp') -> tp')
    | Some (_, tp') -> tp'

  let optimal_proof tp msaux =
    if not (Deque.is_empty msaux.beta_alphas) then
      [snd(Deque.peek_front_exn msaux.beta_alphas)]
    else
      let p1_l = if not (Deque.is_empty msaux.alpha_betas) then
                   [snd(Deque.peek_front_exn msaux.alpha_betas)]
                 else [] in
      let p2_l = if not (Deque.is_empty msaux.alphas_out) then
                   let vp_f2 = snd(Deque.peek_front_exn msaux.alphas_out) in
                   match vp_f2 with
                   | V f2 -> [V (VSince (tp, f2, []))]
                   | S _ -> raise VEXPL
                 else [] in
      let p3_l = if (Deque.length msaux.betas_suffix_in) = (Deque.length msaux.ts_tp_in) then
                   let etp = match Deque.is_empty msaux.betas_suffix_in with
                     | true -> etp msaux.ts_tp_in msaux.ts_tp_out tp
                     | false -> v_at (snd(Deque.peek_front_exn msaux.betas_suffix_in)) in
                   let betas_suffix = betas_suffix_in_to_list msaux.betas_suffix_in in
                   [V (VSinceInf (tp, etp, betas_suffix))]
                 else [] in
      (p1_l @ p2_l @ p3_l)

  let add_to_msaux ts p1 p2 msaux le =
    match p1, p2 with
    | S sp1, S sp2 ->
       (* beta_alphas *)
       let beta_alphas = sappend_to_deque sp1 msaux.beta_alphas in
       (* beta_alphas_out *)
       let beta_alphas_out = sappend_to_deque sp1 msaux.beta_alphas_out in
       let sp = S (SSince (sp2, [])) in
       let () = Deque.enqueue_back beta_alphas_out (ts, sp) in
       (* alphas_betas_out *)
       let () = Deque.enqueue_back msaux.alphas_betas_out (ts, None, None) in
       { msaux with beta_alphas; beta_alphas_out }
    | S sp1, V vp2 ->
       (* beta_alphas *)
       let beta_alphas = sappend_to_deque sp1 msaux.beta_alphas in
       let beta_alphas_out = sappend_to_deque sp1 msaux.beta_alphas_out in
       (* alphas_betas_out *)
       let () = Deque.enqueue_back msaux.alphas_betas_out (ts, None, Some(vp2)) in
       { msaux with beta_alphas; beta_alphas_out }
    | V vp1, S sp2 ->
       (* beta_alphas *)
       let () = Deque.clear msaux.beta_alphas in
       (* beta_alphas_out *)
       let () = Deque.clear msaux.beta_alphas_out in
       let sp = S (SSince (sp2, [])) in
       let () = Deque.enqueue_back msaux.beta_alphas_out (ts, sp) in
       (* alphas_out *)
       let alphas_out = add_alphas_out ts (V vp1) msaux.alphas_out le in
       (* alphas_betas_out *)
       let () = Deque.enqueue_back msaux.alphas_betas_out (ts, Some(vp1), None) in
       { msaux with alphas_out }
    | V vp1, V vp2 ->
       (* beta_alphas *)
       let () = Deque.clear msaux.beta_alphas in
       (* beta_alphas_out *)
       let () = Deque.clear msaux.beta_alphas_out in
       (* alphas_out *)
       let alphas_out = add_alphas_out ts (V vp1) msaux.alphas_out le in
       (* alphas_betas_out *)
       let () = Deque.enqueue_back msaux.alphas_betas_out (ts, Some(vp1), Some(vp2)) in
       { msaux with alphas_out }

  let remove_from_msaux (l, r) msaux =
    let beta_alphas = remove_if_pred_front (fun (ts, _) -> ts < l) msaux.beta_alphas in
    let alpha_betas = remove_if_pred_front (fun (ts, _) -> ts < l) msaux.alpha_betas in
    let alphas_out = remove_if_pred_front (fun (ts, _) -> ts <= r) msaux.alphas_out in
    let betas_suffix_in = remove_if_pred_front (fun (ts, _) -> ts < l) msaux.betas_suffix_in in
    { msaux with beta_alphas
               ; alpha_betas
               ; alphas_out
               ; betas_suffix_in }

 let advance_msaux (l, r) tp ts p1 p2 msaux le =
    let msaux_plus_new = add_to_msaux ts p1 p2 msaux le in
    let msaux_minus_old = remove_from_msaux (l, r) msaux_plus_new in
    let beta_alphas_out, new_in_sat = split_in_out (fun (ts, _) -> ts) (l, r) msaux_minus_old.beta_alphas_out in
    let beta_alphas = update_beta_alphas new_in_sat msaux_minus_old.beta_alphas le in
    let alphas_betas_out, new_in_viol = split_in_out (fun (ts, _, _) -> ts) (l, r) msaux_minus_old.alphas_betas_out in
    let betas_suffix_in = update_betas_suffix_in (Deque.to_list new_in_viol) msaux_minus_old.betas_suffix_in in
    let alpha_betas = update_alpha_betas tp new_in_viol msaux_minus_old.alpha_betas le in
    { msaux_minus_old with beta_alphas
                         ; beta_alphas_out
                         ; alpha_betas
                         ; betas_suffix_in }

  let update_since interval tp ts p1 p2 msaux le =
    let a = get_a_I interval in
    (* Case 1: interval has not yet started, i.e.,
     \tau_{tp} < (\tau_{0} + a) OR (\tau_{tp} - a) < 0 *)
    if ((Option.is_none msaux.ts_zero) && (ts - a) < 0) ||
         (Option.is_some msaux.ts_zero) && ts < (Option.get msaux.ts_zero) + a then
      let l = (-1) in
      let r = (-1) in
      let msaux_ts_zero_updated = if Option.is_none msaux.ts_zero then
                                    { msaux with ts_zero = Some(ts) }
                                  else msaux in
      let msaux_ts_updated = update_ts (l, r) a ts tp msaux_ts_zero_updated in
      let msaux_updated = advance_msaux (l, r) tp ts p1 p2 msaux_ts_updated le in
      let p = V (VSinceOutL tp) in
      ([p], msaux_updated)
    (* Case 2: there exists a \tau_{tp'} inside the interval s.t. tp' < tp *)
    else
      let b = get_b_I interval in
      let l = if (Option.is_some b) then max 0 (ts - (Option.get b))
              else (Option.get msaux.ts_zero) in
      let r = ts - a in
      let msaux_ts_updated = update_ts (l, r) a ts tp msaux in
      let msaux_updated = advance_msaux (l, r) tp ts p1 p2 msaux_ts_updated le in
      (optimal_proof tp msaux_updated, msaux_updated)
end

module Future = struct
  type muaux = {
      ts_tp_in: (timestamp * timepoint) Deque.t
    ; ts_tp_out: (timestamp * timepoint) Deque.t
    (* deque of sorted deques of U^+ beta [alphas] proofs where (ets, lts, expl):
     * etc corresponds to the timestamp of the first alpha proof
     * lts corresponds to the timestamp of the beta proof *)
    ; alphas_beta: ((timestamp * timestamp * expl) Deque.t) Deque.t
    (* most recent sequence of alpha satisfactions w/o holes *)
    ; alphas_suffix: (timestamp * sexpl) Deque.t
    (* deque of sorted deques of U^- ~alpha [~betas] proofs where (ets, lts, expl):
     * ets corresponds to the timestamp of the first ~beta proof
     * lts corresponds to the timestamp of the ~alpha proof *)
    ; betas_alpha: ((timestamp * timestamp * expl) Deque.t) Deque.t
    (* sorted deque of alpha proofs outside the interval *)
    ; alphas_out: (timestamp * expl) Deque.t
    (* deque of alpha violations inside the interval *)
    ; alphas_in: (timestamp * expl) Deque.t
    (* deque of beta violations inside the interval *)
    ; betas_suffix_in: (timestamp * timepoint * vexpl option) Deque.t
    ; optimal_proofs: (timestamp * expl) Deque.t
    ; }

  let muaux_to_string { ts_tp_in
                      ; ts_tp_out
                      ; alphas_beta
                      ; alphas_suffix
                      ; betas_alpha
                      ; alphas_out
                      ; alphas_in
                      ; betas_suffix_in
                      ; optimal_proofs } =
    "\n\nmuaux: " ^
      Deque.fold ts_tp_in ~init:"\nts_tp_in = ["
        ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d,%d);" ts tp)) ^
      (Printf.sprintf "]\n") ^
      Deque.fold ts_tp_out ~init:"\nts_tp_out = ["
        ~f:(fun acc (ts, tp) -> acc ^ (Printf.sprintf "(%d,%d);" ts tp)) ^
      (Printf.sprintf "]\n") ^
      Deque.foldi alphas_beta ~init:"\nalphas_beta = \n"
        ~f:(fun i acc1 d ->
          acc1 ^ Printf.sprintf "\n%d.\n" i ^
          Deque.fold d ~init:"["
            ~f:(fun acc2 (ts1, ts2, ps) ->
              acc2 ^ (Printf.sprintf "\n(%d, %d)\n" ts1 ts2) ^
              Expl.expl_to_string ps) ^ "\n]\n") ^
      Deque.fold alphas_suffix ~init:"\nalphas_suffix = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.s_to_string "" ps) ^
      Deque.foldi betas_alpha ~init:"\nbetas_alpha = \n"
        ~f:(fun i acc1 d ->
          acc1 ^ Printf.sprintf "\n%d.\n" i ^
          Deque.fold d ~init:"["
            ~f:(fun acc2 (ts1, ts2, ps) ->
              acc2 ^ (Printf.sprintf "\n(%d, %d)\n" ts1 ts2) ^
              Expl.expl_to_string ps) ^ "\n]\n") ^
      Deque.fold alphas_out ~init:"\nalphas_out = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold alphas_in ~init:"\nalphas_in = "
        ~f:(fun acc (ts, ps) ->
          acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) ^
      Deque.fold betas_suffix_in ~init:"\nbetas_suffix_in = "
        ~f:(fun acc (ts, tp, ps) ->
          match ps with
          | None -> acc ^ (Printf.sprintf "\n(%d, %d) None\n" ts tp)
          | Some(p) -> acc ^ (Printf.sprintf "\n(%d, %d)\n" ts tp) ^ Expl.v_to_string "" p)

  let ts_of_tp tp muaux =
    match (Deque.find muaux.ts_tp_out ~f:(fun (ts', tp') -> tp = tp')) with
    | None -> (match (Deque.find muaux.ts_tp_in ~f:(fun (ts', tp') -> tp = tp')) with
               | None -> raise (NOT_FOUND "ts not found")
               | Some(ts, _) -> ts)
    | Some(ts, _) -> ts

  let step_sdrop_tp tp alphas_beta =
    Deque.fold alphas_beta ~init:(Deque.create ())
               ~f:(fun acc (ets, lts, ssp) ->
                 (match ssp with
                  | S sp -> if tp = (s_at sp) then
                              (match sdrop sp with
                               | None -> acc
                               | Some (sp') -> let () = Deque.enqueue_back acc (ets, lts, S sp') in acc)
                            else let () = Deque.enqueue_back acc (ets, lts, ssp) in acc
                  | V _ -> raise SEXPL))

  let step_vdrop_ts a ts betas_alpha muaux =
    (* let () = Printf.printf "HI1!\n" in *)
    let rec vdrop_until vp =
      let () = Printf.printf "tp = %d; ts_of_tp = %d; ts + a = %d\n" (v_at vp) (ts_of_tp (v_at vp) muaux) (ts + a) in
      if (ts_of_tp (v_etp vp) muaux) < (ts + a) then
        (match vdrop vp with
         | None -> None
         | Some(vp') -> vdrop_until vp')
      else Some(vp) in
    Deque.fold betas_alpha ~init:(Deque.create ())
      ~f:(fun acc (ets, lts, vvp) ->
        (match vvp with
         | V vp -> (match vdrop_until vp with
                    | None -> acc
                    | Some (vp') -> let () = Deque.enqueue_back acc (ets, lts, V vp') in acc)
         | S _ -> raise VEXPL))

  let remove_out_less2_lts lim d =
    let () = Deque.iteri d ~f:(fun i d' ->
                 Deque.set_exn d i
                   (Deque.fold d' ~init:(Deque.create ())
                      ~f:(fun acc (ets, lts, p) ->
                        let () = if lts >= lim then Deque.enqueue_back acc (ets, lts, p) in acc))) in
    let _ = remove_if_pred_front_ne (fun d' -> Deque.is_empty d') d in
    d

  let remove_step_muaux a ts tp muaux =
    (* ts_tp_in *)
    let ts_tp_in = remove_if_pred_front (fun (_, tp') -> tp = tp') muaux.ts_tp_in in
    (* ts_tp_out *)
    let ts_tp_out = remove_if_pred_front (fun (_, tp') -> tp = tp') muaux.ts_tp_out in
    (* alphas_beta *)
    let () = Deque.iteri muaux.alphas_beta
               ~f:(fun i d -> Deque.set_exn muaux.alphas_beta i (remove_if_pred_front (fun (_, _, p) -> tp = (p_at p)) d)) in
    let alphas_beta = remove_if_pred_front_ne (fun d' -> Deque.is_empty d') muaux.alphas_beta in
    (* alphas_suffix *)
    let alphas_suffix = remove_if_pred_front (fun (_, p) -> tp = (s_at p)) muaux.alphas_suffix in
    (* betas_alpha *)
    let () = Deque.iteri muaux.betas_alpha
               ~f:(fun i d -> Deque.set_exn muaux.betas_alpha i (remove_if_pred_front (fun (_, _, p) -> tp = (p_at p)) d)) in
    let betas_alpha = remove_if_pred_front_ne (fun d' -> Deque.is_empty d') muaux.betas_alpha in
    (* alphas_out *)
    let alphas_out = remove_if_pred_front (fun (_, p) -> tp = (p_at p)) muaux.alphas_out in
    (* betas_suffix_in *)
    let betas_suffix_in = remove_if_pred_front (fun (_, tp', _) -> tp = tp') muaux.betas_suffix_in in
    { muaux with ts_tp_in
               ; ts_tp_out
               ; alphas_beta
               ; alphas_suffix
               ; betas_alpha
               ; alphas_out
               ; betas_suffix_in }

  let remove_muaux (z, l) muaux le = muaux
    (* let alphas_in, new_out_alphas = split_in_out2 (fun (ts, _) -> ts) (z, l) muaux.alphas_in in
     * let alphas_out = sorted_append new_out_alphas muaux.alphas_out le in
     * (\* alphas_beta *\)
     * let alphas_beta = remove_out_less2_lts l muaux.alphas_beta in
     * { muaux with alphas_beta
     *            ; alphas_in
     *            ; alphas_out } *)

  let alphas_suffix_to_list alphas_suffix =
    List.rev(List.fold_left (Deque.to_list alphas_suffix) ~init:[]
               ~f:(fun acc (_, sp1) -> sp1::acc))

  let betas_suffix_in_to_list betas_suffix_in =
    List.rev(Deque.fold betas_suffix_in ~init:[]
               ~f:(fun acc (ts, tp, vp2_opt) -> match vp2_opt with
                                                | None -> []
                                                | Some(vp2) -> vp2::acc))

  let first_somes_betas_suffix_in_to_list betas_suffix_in =
    List.rev(Deque.fold_until betas_suffix_in ~init:[]
               ~f:(fun acc (ts, tp, vp2_opt) -> match vp2_opt with
                                                | None -> Stop acc
                                                | Some(vp2) -> Continue (vp2::acc))
               ~finish:(fun acc -> acc))

  let add_p1_p2 lts tp p1 p2 muaux le =
    match p1, p2 with
    | S sp1, S sp2 ->
       Printf.printf "SS\n";
       (* alphas_beta *)
       let cur_alphas_beta = Deque.peek_back_exn muaux.alphas_beta in
       let sp = S (SUntil (sp2, (alphas_suffix_to_list muaux.alphas_suffix))) in
       let ets = match Deque.peek_front muaux.alphas_suffix with
         | Some(ts, _) -> ts
         | None -> lts in
       let cur_alphas_beta_sorted = sorted_enqueue (ets, lts, sp) cur_alphas_beta le in
       let _ = Deque.drop_back muaux.alphas_beta in
       let _ = Deque.enqueue_back muaux.alphas_beta cur_alphas_beta_sorted in
       (* betas_alpha (add empty deque) *)
       let _ = if not (Deque.is_empty (Deque.peek_back_exn muaux.betas_alpha)) then
                 Deque.enqueue_back muaux.betas_alpha (Deque.create ()) in
       (* alphas_suffix *)
       let _ = Deque.enqueue_back muaux.alphas_suffix (lts, sp1) in
       (* betas_suffix_in *)
       let _ = Deque.enqueue_back muaux.betas_suffix_in (lts, tp, None) in
       muaux
    | S sp1, V vp2 ->
       Printf.printf "SV\n";
       (* alphas_suffix *)
       let _ = Deque.enqueue_back muaux.alphas_suffix (lts, sp1) in
       (* betas_suffix_in *)
       let _ = Deque.enqueue_back muaux.betas_suffix_in (lts, tp, Some(vp2)) in
       muaux
    | V vp1, S sp2 ->
       Printf.printf "VS\n";
       (* alphas_beta *)
       let cur_alphas_beta = Deque.peek_back_exn muaux.alphas_beta in
       let sp = S (SUntil (sp2, (alphas_suffix_to_list muaux.alphas_suffix))) in
       let ets = match Deque.peek_front muaux.alphas_suffix with
         | Some(ts, _) -> ts
         | None -> lts in
       let cur_alphas_beta_sorted = sorted_enqueue (ets, lts, sp) cur_alphas_beta le in
       let _ = Deque.drop_back muaux.alphas_beta in
       let _ = Deque.enqueue_back muaux.alphas_beta cur_alphas_beta_sorted in
       (* alphas_beta (append empty deque) *)
       let _ = if not (Deque.is_empty (Deque.peek_back_exn muaux.alphas_beta)) then
                 Deque.enqueue_back muaux.alphas_beta (Deque.create ()) in
       (* betas_alpha (add empty deque) *)
       let _ = if not (Deque.is_empty (Deque.peek_back_exn muaux.betas_alpha)) then
                 Deque.enqueue_back muaux.betas_alpha (Deque.create ()) in
       (* alphas_suffix *)
       let _ = Deque.clear muaux.alphas_suffix in
       (* alphas_in *)
       let _ = Deque.enqueue_back muaux.alphas_in (lts, V vp1) in
       (* betas_suffix_in *)
       let _ = Deque.enqueue_back muaux.betas_suffix_in (lts, tp, None) in
       muaux
    | V vp1, V vp2 ->
       Printf.printf "VV\n";
       (* alphas_beta (add empty deque) *)
       let _ = if not (Deque.is_empty (Deque.peek_back_exn muaux.alphas_beta)) then
                 Deque.enqueue_back muaux.alphas_beta (Deque.create ()) in
       (* alphas_suffix *)
       let _ = Deque.clear muaux.alphas_suffix in
       (* betas_suffix_in *)
       let _ = Deque.enqueue_back muaux.betas_suffix_in (lts, tp, Some(vp2)) in
       (* betas_alpha *)
       let cur_betas_alpha = Deque.peek_back_exn muaux.betas_alpha in
       let vp = V (VUntil (tp, vp1, (betas_suffix_in_to_list muaux.betas_suffix_in))) in
       let ets = match Deque.peek_front muaux.betas_suffix_in with
         | Some(ts, tp, _) -> ts
         | None -> lts in
       let cur_betas_alpha_sorted = sorted_enqueue (ets, lts, vp) cur_betas_alpha le in
       let _ = Deque.drop_back muaux.betas_alpha in
       let _ = Deque.enqueue_back muaux.betas_alpha cur_betas_alpha_sorted in
       (* alphas_in *)
       let _ = Deque.enqueue_back muaux.alphas_in (lts, V vp1) in
       muaux

  let drop_alphas_beta_tp tp alphas_beta =
    let _ = (match Deque.peek_front alphas_beta with
             | None -> raise (EMPTY_DEQUE "alphas_beta")
             | Some(front_alphas_beta) -> if not (Deque.is_empty front_alphas_beta) then
                                            let () = Deque.drop_front alphas_beta in
                                            let front_alphas_beta' = step_sdrop_tp tp front_alphas_beta in
                                            if not (Deque.is_empty front_alphas_beta') then
                                              Deque.enqueue_front alphas_beta front_alphas_beta'
                                            else (if Deque.is_empty alphas_beta then
                                                    Deque.enqueue_front alphas_beta front_alphas_beta')) in
    alphas_beta

  let drop_betas_alpha_ts a ts muaux =
    (* let () = Printf.printf "HI2!\n" in *)
    let _ = (match Deque.peek_front muaux.betas_alpha with
             | None -> raise (EMPTY_DEQUE "betas_alpha")
             | Some(front_betas_alpha) -> if not (Deque.is_empty front_betas_alpha) then
                                            let () = Deque.drop_front muaux.betas_alpha in
                                            let front_betas_alpha' = step_vdrop_ts a ts front_betas_alpha muaux in
                                            if not (Deque.is_empty front_betas_alpha') then
                                              Deque.enqueue_front muaux.betas_alpha front_betas_alpha'
                                            else (if Deque.is_empty muaux.betas_alpha then
                                                    Deque.enqueue_front muaux.betas_alpha front_betas_alpha')) in
    muaux.betas_alpha

  let drop_muaux_tp tp muaux =
    let alphas_beta = drop_alphas_beta_tp tp muaux.alphas_beta in
    { muaux with alphas_beta }

  let drop_muaux_ts a ts muaux =
    let betas_alpha = drop_betas_alpha_ts a ts muaux in
    { muaux with betas_alpha }

  let ready_tss_tps ts_tp_out ts_tp_in nts b =
    let d = Deque.create () in
    let _ = Deque.iter ts_tp_out ~f:(fun (ts, tp) ->
                if ts + b < nts then Deque.enqueue_back d (ts, tp)) in
    let _ = Deque.iter ts_tp_in ~f:(fun (ts, tp) ->
                if ts + b < nts then Deque.enqueue_back d (ts, tp)) in
    d

  let first_ts_tp muaux =
    match Deque.peek_front muaux.ts_tp_out with
    | None -> (match Deque.peek_front muaux.ts_tp_in with
               | None -> raise (NOT_FOUND "first_ts_tp could not find a (ts, tp)")
               | Some (ts, tp) -> (ts, tp))
    | Some (ts, tp) -> (ts, tp)

  let drop_first_ts_tp muaux =
    match Deque.peek_front muaux.ts_tp_out with
    | None -> Deque.drop_front muaux.ts_tp_in
    | Some (_) -> Deque.drop_front muaux.ts_tp_out

  let adjust_ts_tp a ts muaux =
    let () = Deque.iter muaux.ts_tp_in
               ~f:(fun (ts', tp') ->
                 if ts' < ts + a then
                   (* let _ = drop_betas_alpha_tp tp muaux.betas_alpha in *)
                   Deque.enqueue_back muaux.ts_tp_out (ts', tp')) in
    let _ = remove_if_pred_front (fun (ts', _) -> ts' < ts + a) muaux.ts_tp_in in
    let _ = remove_if_pred_front (fun (ts', _) -> ts' < ts) muaux.ts_tp_out in
    muaux

  let adjust_muaux a muaux le =
    let () = drop_first_ts_tp muaux in
    let (ts, tp) = first_ts_tp muaux in
    (* ts_tp_out and ts_tp_out *)
    let muaux = adjust_ts_tp a ts muaux in
    (* alphas_beta *)
    let muaux = drop_muaux_tp tp muaux in
    let () = Deque.iteri muaux.alphas_beta ~f:(fun i d ->
                 Deque.set_exn muaux.alphas_beta i (remove_if_pred_front (fun (_, lts, _) -> lts < ts + a) d)) in
    let alphas_beta = remove_if_pred_front_ne (fun d' -> Deque.is_empty d') muaux.alphas_beta in
    (* betas_alpha *)
    let muaux = drop_muaux_ts a ts muaux in
    let () = Deque.iteri muaux.betas_alpha ~f:(fun i d ->
                 Deque.set_exn muaux.betas_alpha i (remove_if_pred_front (fun (_, lts, _) ->
                                                        let () = Printf.printf "lts = %d; ts + a = %d\n" lts (ts+a) in
                                                        lts < ts + a) d)) in
    let betas_alpha = remove_if_pred_front_ne (fun d' -> Deque.is_empty d') muaux.betas_alpha in
    (* alphas_in and alphas_out *)
    let alphas_in, new_out_alphas = split_out_in (fun (ts', _) -> ts') (ts, (ts + a)) muaux.alphas_in in
    let () = Printf.printf "\nnew_out_alphas = \n" in
    let () = Deque.iter new_out_alphas ~f:(fun (ts, p) -> Printf.printf "%s\n" (Expl.expl_to_string p)) in
    let alphas_out_plus = sorted_append new_out_alphas muaux.alphas_out le in
    let () = Printf.printf "\nalphas_out_plus = \n" in
    let () = Deque.iter alphas_out_plus ~f:(fun (ts, p) -> Printf.printf "%s\n" (Expl.expl_to_string p)) in
    let alphas_out = remove_if_pred_front (fun (_, p) -> (p_at p) <= tp) alphas_out_plus in
    (* betas_suffix_in *)
    let betas_suffix_in = remove_if_pred_front (fun (ts', _, _) -> ts' < ts + a) muaux.betas_suffix_in in
    { muaux with alphas_beta
               ; betas_alpha
               ; alphas_in
               ; alphas_out
               ; betas_suffix_in}

  let eval_step_muaux a ts tp muaux le minimuml =
    let _ = Printf.printf "eval_step_muaux ts = %d; tp = %d\n" ts tp in
    let optimal_proofs_len = Deque.length muaux.optimal_proofs in
    (* let () = Printf.printf "after drop_muaux_ts\n" in
     * let () = match Deque.peek_front muaux.betas_alpha with
     *   | None -> ()
     *   | Some(d) -> Deque.iter d ~f:(fun (_, _, p) -> Printf.printf "tp = %d; p = %s\n" (p_at p) (Expl.expl_to_string p)) in *)
    (* U^+ (satisfaction) case *)
    let () = let cur_alphas_beta = Deque.peek_front_exn muaux.alphas_beta in
             let () = (if not (Deque.is_empty cur_alphas_beta) then
                         ((* let () = Printf.printf "cur_alphas_beta =\n" in
                           * let () = Deque.iter cur_alphas_beta ~f:(fun (ts, tp, p) -> Printf.printf "%s\n" (Expl.expl_to_string p)) in *)
                          match Deque.peek_front_exn cur_alphas_beta with
                          | (_, _, S sp) -> if tp = (s_at sp) then Deque.enqueue_back muaux.optimal_proofs (ts, S sp)
                          | _ -> raise VEXPL)) in
             (* U^-/U_{\infty}^- (violation) cases *)
             if (Deque.length muaux.optimal_proofs) = optimal_proofs_len then
               (let p1_l = if not (Deque.is_empty muaux.betas_alpha) then
                             let cur_betas_alpha = Deque.peek_front_exn muaux.betas_alpha in
                             (if not (Deque.is_empty cur_betas_alpha) then
                                match Deque.peek_front_exn cur_betas_alpha with
                                | (_, _, V VUntil(tp', vp1, vp2s)) -> (match Deque.peek_front muaux.ts_tp_in with
                                                                     | None -> []
                                                                     | Some(_, first_tp_in) -> if tp' = first_tp_in then
                                                                                                 [V (VUntil(tp, vp1, vp2s))]
                                                                                               else [])
                                | _ -> raise (INVALID_EXPL "Explanation should be VUntil")
                              else [])
                           else [] in
                let p2_l = if not (Deque.is_empty muaux.alphas_out) then
                             let vvp1 = snd(Deque.peek_front_exn muaux.alphas_out) in
                             match vvp1 with
                             | V vp1 -> [V (VUntil (tp, vp1, []))]
                             | S _ -> raise VEXPL
                           else [] in
                let p3_l = let betas_suffix = first_somes_betas_suffix_in_to_list muaux.betas_suffix_in in
                           if (List.length betas_suffix) > 0 &&
                                (List.length betas_suffix) = (Deque.length muaux.ts_tp_in) then
                             let (_, ltp, _) = Deque.peek_back_exn muaux.betas_suffix_in in
                             (* let _ = Printf.printf "|betas_suffix_in| = %d; ltp = %d\n" (List.length betas_suffix) ltp in *)
                             [V (VUntilInf (tp, ltp, betas_suffix))]
                           else [] in
                (* let _ = Printf.printf "Possible proofs:\n" in
                 * let _ = List.iter (p1_l @ p2_l @ p3_l) ~f:(fun p -> Printf.printf "%s\n" (Expl.expl_to_string p)) in *)
                Deque.enqueue_back muaux.optimal_proofs (ts, minimuml (p1_l @ p2_l @ p3_l))) in
    let _ = Deque.iter muaux.optimal_proofs ~f:(fun (ts, p) -> Printf.printf "%s\n" (Expl.expl_to_string p)) in
    let muaux = adjust_muaux a muaux le in
    (* let muaux_minus_tp = remove_step_muaux a ts tp muaux_dropped_tp in *)
    muaux

  let shift_muaux (a, b) nts muaux le minimuml =
    let tss_tps = ready_tss_tps muaux.ts_tp_out muaux.ts_tp_in nts b in
    Deque.fold tss_tps ~init:muaux
      ~f:(fun acc (ts, tp) -> if ts + b < nts then eval_step_muaux a ts tp acc le minimuml
                              else acc)

  (* let ets muaux ts =
   *   match Deque.peek_front muaux.ts_tp_out with
   *   | None -> (match Deque.peek_front muaux.ts_tp_in with
   *              | None -> ts
   *              | Some (ts', _) -> ts')
   *   | Some (ts', _) -> ts' *)

  let update_until interval nts tp p1 p2 muaux le minimuml =
    let a = get_a_I interval in
    let b = match get_b_I interval with
      | None -> raise UNBOUNDED_FUTURE
      | Some(b') -> b' in
    let muaux = shift_muaux (a, b) nts muaux le minimuml in
    let muaux = add_p1_p2 nts tp p1 p2 muaux le in
    let () = if ((Deque.length muaux.ts_tp_out) + (Deque.length muaux.ts_tp_in)) > 0 then
               (let (ts, _) = first_ts_tp muaux in
                (if nts < ts + a then Deque.enqueue_back muaux.ts_tp_out (nts, tp)
                 else Deque.enqueue_back muaux.ts_tp_in (nts, tp)))
             else (if nts >= a && nts <= b then Deque.enqueue_back muaux.ts_tp_in (nts, tp)
                   else Deque.enqueue_back muaux.ts_tp_out (nts, tp)) in
    (* let () = Deque.enqueue_back muaux.ts_tp_in (nts, tp) in *)
    (* let z = max 0 (ets muaux ts) in
     * let l = max 0 ((ets muaux ts) + a) in
     * Printf.fprintf stdout "z = %d; l = %d; r = %d\n" z l ts; *)
    (* let muaux_minus_old = remove_muaux (z, l) muaux_plus_p1_p2 le in *)
    muaux

  let rec eval_until d interval nts muaux =
    (* let _ = Printf.printf "|muaux.optimal_proofs| = %d\n" (Deque.length muaux.optimal_proofs) in *)
    let b = match get_b_I interval with
      | None -> raise UNBOUNDED_FUTURE
      | Some(b') -> b' in
    match Deque.peek_back muaux.optimal_proofs with
    | None -> (d, muaux)
    | Some(ts, _) -> if ts + b < nts then
                       (* let _ = Printf.printf "should output something\n" in *)
                       let (_, op) = Deque.dequeue_back_exn muaux.optimal_proofs in
                       let (ops, muaux) = eval_until d interval nts muaux in
                       let _ = Deque.enqueue_back ops op in
                       (ops, muaux)
                     else (d, muaux)
end

(* mbuf2: auxiliary data structure for binary operators *)
type mbuf2 = expl Deque.t * expl Deque.t

let mbuf2_add p1s p2s (d1, d2) =
  let _ = Deque.iter p1s ~f:(fun p1 -> Deque.enqueue_front d1 p1) in
  let _ = Deque.iter p2s ~f:(fun p2 -> Deque.enqueue_front d2 p2) in
  (d1, d2)

let rec mbuf2_take f (p1s, p2s) =
  match (Deque.is_empty p1s, Deque.is_empty p2s) with
  | true, _ -> (Deque.create (), (p1s, p2s))
  | _, true -> (Deque.create (), (p1s, p2s))
  | false, false -> let p1 = Deque.dequeue_front_exn p1s in
                    let p2 = Deque.dequeue_front_exn p2s in
                    let (p3s, buf') = mbuf2_take f (p1s, p2s) in
                    let _ = Deque.enqueue_front p3s (f p1 p2) in
                    (p3s, buf')

let rec mbuf2t_take f z (p1s, p2s) tss_tps =
  match (Deque.is_empty p1s, Deque.is_empty p2s, Deque.is_empty tss_tps) with
  | true, _, _ -> (z, (p1s, p2s), tss_tps)
  | _, true, _ -> (z, (p1s, p2s), tss_tps)
  | _, _, true -> (z, (p1s, p2s), tss_tps)
  | false, false, false -> let p1 = Deque.dequeue_front_exn p1s in
                           let p2 = Deque.dequeue_front_exn p2s in
                           let (ts, tp) = Deque.dequeue_front_exn tss_tps in
                           mbuf2t_take f (f p1 p2 ts tp z) (p1s, p2s) tss_tps

type mformula =
  | MTT
  | MFF
  | MP of string
  | MNeg of mformula
  | MConj of mformula * mformula * mbuf2
  | MDisj of mformula * mformula * mbuf2
  | MPrev of interval * mformula * bool * expl list * timestamp list
  | MNext of interval * mformula * bool * timestamp list
  | MSince of interval * mformula * mformula * mbuf2 * (timestamp * timepoint) Deque.t * Past.msaux
  | MUntil of interval * mformula * mformula * mbuf2 * (timestamp * timepoint) Deque.t * Future.muaux

let rec mformula_to_string l f =
  match f with
  | MP x -> Printf.sprintf "%s" x
  | MTT -> Printf.sprintf "⊤"
  | MFF -> Printf.sprintf "⊥"
  | MConj (f, g, _) -> Printf.sprintf (paren l 4 "%a ∧ %a") (fun x -> mformula_to_string 4) f (fun x -> mformula_to_string 4) g
  | MDisj (f, g, _) -> Printf.sprintf (paren l 3 "%a ∨ %a") (fun x -> mformula_to_string 3) f (fun x -> mformula_to_string 4) g
  | MNeg f -> Printf.sprintf "¬%a" (fun x -> mformula_to_string 5) f
  | MPrev (i, f, _, _, _) -> Printf.sprintf (paren l 5 "●%a %a") (fun x -> interval_to_string) i (fun x -> mformula_to_string 5) f
  | MNext (i, f, _, _) -> Printf.sprintf (paren l 5 "○%a %a") (fun x -> interval_to_string) i (fun x -> mformula_to_string 5) f
  | MSince (i, f, g, _, _, _) -> Printf.sprintf (paren l 0 "%a S%a %a") (fun x -> mformula_to_string 5) f (fun x -> interval_to_string) i (fun x -> mformula_to_string 5) g
  | MUntil (i, f, g, _, _, _) -> Printf.sprintf (paren l 0 "%a U%a %a") (fun x -> mformula_to_string 5) f (fun x -> interval_to_string) i (fun x -> mformula_to_string 5) g
let mformula_to_string = mformula_to_string 0

let relevant_ap mf =
  let rec aux mf =
    match mf with
    | MP x -> [x]
    | MTT -> []
    | MFF -> []
    | MConj (f, g, _) -> aux f @ aux g
    | MDisj (f, g, _) -> aux f @ aux g
    | MNeg f -> aux f
    | MPrev (i, f, _, _, _) -> aux f
    | MNext (i, f, _, _) -> aux f
    | MSince (i, f, g, _, _, _) -> aux f @ aux g
    | MUntil (i, f, g, _, _, _) -> aux f @ aux g in
  let lst_with_dup = aux mf in
  List.fold_left lst_with_dup ~init:[] ~f:(fun acc s ->
      if (List.mem acc s ~equal:(fun x y -> x = y)) then acc
      else s::acc)

let filter_ap sap mf_ap =
  Util.SS.filter (fun s -> List.mem mf_ap s ~equal:(fun x y -> x = y)) sap

type context =
  { tp: timepoint
  ; mf: mformula
  ; events: (Util.SS.t * timestamp) list
  }

let print_ps_list l =
  List.iter l ~f:(fun (ts, p) -> Printf.fprintf stdout "%s\n" (expl_to_string p))

(* Convert formula into a formula state *)
let rec minit f =
  match (value f) with
  | TT -> MTT
  | FF -> MFF
  | P (x) -> MP (x)
  | Neg (f) -> MNeg (minit f)
  | Conj (f, g) ->
     let buf = (Deque.create (), Deque.create ()) in
     MConj (minit f, minit g, buf)
  | Disj (f, g) ->
     let buf = (Deque.create (), Deque.create ()) in
     MDisj (minit f, minit g, buf)
  | Prev (i, f) -> MPrev (i, minit f, true, [], [])
  | Next (i, f) -> MNext (i, minit f, true, [])
  | Since (i, f, g) ->
     let buf = (Deque.create (), Deque.create ()) in
     let msaux = { Past.ts_zero = None
                 ; ts_tp_in = Deque.create ()
                 ; ts_tp_out = Deque.create ()
                 ; beta_alphas = Deque.create ()
                 ; beta_alphas_out = Deque.create ()
                 ; alpha_betas = Deque.create ()
                 ; alphas_out = Deque.create ()
                 ; betas_suffix_in = Deque.create ()
                 ; alphas_betas_out = Deque.create ()
                 ; } in
     MSince (i, minit f, minit g, buf, Deque.create (), msaux)
  | Until (i, f, g) ->
     let buf = (Deque.create (), Deque.create ()) in
     let empty_d1 = Deque.create () in
     let empty_d2 = Deque.create () in
     let alphas_beta = Deque.create () in
     let betas_alpha = Deque.create () in
     let _ = Deque.enqueue_front alphas_beta empty_d1 in
     let _ = Deque.enqueue_front betas_alpha empty_d2 in
     let muaux = { Future.ts_tp_in = Deque.create ()
                 ; ts_tp_out = Deque.create ()
                 ; alphas_beta = alphas_beta
                 ; alphas_suffix = Deque.create ()
                 ; betas_alpha = betas_alpha
                 ; alphas_out = Deque.create ()
                 ; alphas_in = Deque.create ()
                 ; betas_suffix_in = Deque.create ()
                 ; optimal_proofs = Deque.create ()
                 } in
     MUntil (i, minit f, minit g, buf, Deque.create (), muaux)
  | _ -> failwith "This formula cannot be monitored"

let do_disj minimum2 expl_f1 expl_f2 =
  match expl_f1, expl_f2 with
  | S f1, S f2 -> minimum2 (S (SDisjL (f1))) (S (SDisjR(f2)))
  | S f1, V _ -> S (SDisjL (f1))
  | V _ , S f2 -> S (SDisjR (f2))
  | V f1, V f2 -> V (VDisj (f1, f2))

let do_conj minimum2 expl_f1 expl_f2 =
  match expl_f1, expl_f2 with
  | S f1, S f2 -> S (SConj (f1, f2))
  | S _ , V f2 -> V (VConjR (f2))
  | V f1, S _ -> V (VConjL (f1))
  | V f1, V f2 -> minimum2 (V (VConjL (f1))) (V (VConjR (f2)))

let meval' tp ts sap mform le minimuml =
  let minimum2 a b = minimuml [a; b] in
  (* TODO: This function should return (Deque.t, mformula) *)
  let rec meval tp ts sap mform =
    match mform with
    | MTT -> let d = Deque.create () in
             let _ = Deque.enqueue_back d (S (STT tp)) in
             (d, MTT)
    | MFF -> let d = Deque.create () in
             let _ = Deque.enqueue_back d (V (VFF tp)) in
             (d, MFF)
    | MP a ->
       let d = Deque.create () in
       let _ = if Util.SS.mem a sap then Deque.enqueue_back d (S (SAtom (tp, a)))
               else Deque.enqueue_back d (V (VAtom (tp, a))) in
       (d, MP a)
    | MNeg (mf) ->
       let (ps, mf') = meval tp ts sap mf in
       let ps' = Deque.fold ps ~init:(Deque.create ())
                   ~f:(fun d p ->
                     match p with
                     | S p' -> let _ = Deque.enqueue_back d (V (VNeg p')) in d
                     | V p' -> let _ = Deque.enqueue_back d (S (SNeg p')) in d) in
       (ps', MNeg(mf'))
    | MConj (mf1, mf2, buf) ->
       let op p1 p2 = do_conj minimum2 p1 p2 in
       let (p1s, mf1') = meval tp ts sap mf1 in
       let (p2s, mf2') = meval tp ts sap mf2 in
       let (ps, buf') = mbuf2_take op (mbuf2_add p1s p2s buf) in
       (ps, MConj (mf1', mf2', buf'))
    | MDisj (mf1, mf2, buf) ->
       let op p1 p2 = do_disj minimum2 p1 p2 in
       let (p1s, mf1') = meval tp ts sap mf1 in
       let (p2s, mf2') = meval tp ts sap mf2 in
       let (ps, buf') = mbuf2_take op (mbuf2_add p1s p2s buf) in
       (ps, MDisj (mf1', mf2', buf'))
    (* | MPrev (interval, mf, b, expl_lst, ts_d_lst) ->
     * | MNext (interval, mf, b, ts_a_lst) -> *)
    | MSince (interval, mf1, mf2, buf, tss_tps, msaux) ->
       let (p1s, mf1') = meval tp ts sap mf1 in
       let (p2s, mf2') = meval tp ts sap mf2 in
       let _ = Deque.enqueue_back tss_tps (ts, tp) in
       let ((ps, msaux'), buf', tss_tps') =
         mbuf2t_take
           (fun p1 p2 ts tp (ps, aux) ->
             let (cps, aux) = Past.update_since interval tp ts p1 p2 msaux le in
             let op = minimuml cps in
             let _ = Deque.enqueue_back ps op in
             (ps, aux))
           (Deque.create (), msaux) (mbuf2_add p1s p2s buf) tss_tps in
       (ps, MSince (interval, mf1', mf2', buf', tss_tps', msaux'))
    | MUntil (interval, mf1, mf2, buf, tss_tps, muaux) ->
       let (p1s, mf1') = meval tp ts sap mf1 in
       let (p2s, mf2') = meval tp ts sap mf2 in
       (* let _ = Printf.printf "---------------\n" in *)
       (* let _ = Printf.printf "mf = %s\n" (mformula_to_string (MUntil (interval, mf1, mf2, buf, tss_tps, muaux))) in *)
       let _ = Deque.enqueue_back tss_tps (ts, tp) in
       let _ = Printf.fprintf stdout "---------------\n%s\n\n" (Future.muaux_to_string muaux) in
       let (muaux', buf', ntss_ntps) =
         mbuf2t_take
           (fun p1 p2 ts tp aux -> Future.update_until interval ts tp p1 p2 muaux le minimuml)
           muaux (mbuf2_add p1s p2s buf) tss_tps in
       let nts = match Deque.peek_front ntss_ntps with
         | None -> ts
         | Some(nts', _) -> nts' in
       let _ = Deque.fold muaux.optimal_proofs ~init:"\noptimal_proofs = "
                 ~f:(fun acc (ts, ps) ->
                   acc ^ (Printf.sprintf "\n(%d)\n" ts) ^ Expl.expl_to_string ps) in
       let (ps, muaux'') = Future.eval_until (Deque.create ()) interval nts muaux in
       let _ = Printf.printf "|ps| = %d\n" (Deque.length ps) in
       (ps, MUntil (interval, mf1', mf2', buf', ntss_ntps, muaux''))
    | _ -> failwith "This formula cannot be monitored" in
  meval tp ts sap mform

let monitor in_ch out_ch mode debug check test le f =
  let minimuml ps = minsize_list (get_mins le ps) in
  let rec loop f x = loop f (f x) in
  let mf = minit f in
  let mf_ap = relevant_ap mf in
  let _ = preamble out_ch mode f in
  let ctx = { tp = 0
            ; mf = mf
            ; events = [] } in
  let s (ctx, in_ch) =
    let ((sap, ts), in_ch) = input_event in_ch out_ch in
    let sap_filtered = filter_ap sap mf_ap in
    let events_updated = (sap_filtered, ts)::ctx.events in
    let (ps, mf_updated) = meval' ctx.tp ts sap_filtered ctx.mf le minimuml in
    (* let _ = Printf.fprintf stdout "|ps| outside = %d\n" (List.length ps) in *)
    let checker_ps = if check || debug || test then Some (check_ps events_updated f (Deque.to_list ps)) else None in
    let _ = print_ps out_ch mode ts ctx.tp (Deque.to_list ps) checker_ps debug test in
    let ctx_updated = { tp = ctx.tp+1
                      ; mf = mf_updated
                      ; events = events_updated } in
    (ctx_updated, in_ch) in
  loop s (ctx, in_ch)
