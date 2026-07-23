open Core
open Sk
open Types

(* Compared to the [lib] version of this module, all logic related to abstract
   data types has been pulled out and generalized, see module [Data_type]. *)

let unpack_tag ty =
  let%bind.Option res = p_type_match (tag (Kop * Ref "t")) ty in
  let%bind.Option tty = Map.find res "t" in
  Some (Tag.of_combinatory_type tty)

let unpack_constructor ty =
  let%bind.Option res = p_type_match (tagged (Ref "f") (Ref "t")) ty in
  let%bind.Option fty = Map.find res "f" in
  let%bind.Option tty = Map.find res "t" in
  let%bind.Option tag = unpack_tag tty in
  Some (fty, tag)

let infer_app fuel =
  let rec app ty uty =
    Int.decr fuel;
    if !fuel = 0 then None
    else
      match ty with
      | Kty -> Some (K1ty uty)
      | K1ty uty1 -> Some uty1
      | Sty -> Some (S1ty uty)
      | S1ty uty1 -> (
          let rty = S2ty (uty1, uty) in
          match unpack_constructor rty with
          | None -> Some rty
          | Some ctor ->
              let%map.Option data = Data_type.infer ~app ctor in
              Data data)
      | S2ty (uty1, vty1) ->
          let%bind.Option vty2 = app vty1 uty in
          let%bind.Option ty2 = app uty1 uty in
          let%bind.Option wty = app ty2 vty2 in
          Some wty
      | Data data -> Data_type.app ~app data uty
  in
  app

let rec infer fuel gamma m =
  match m with
  | Ref x -> get x gamma
  | Sop -> Some Sty
  | Kop -> Some Kty
  | App (m1, m2) -> (
      match (infer fuel gamma m1, infer fuel gamma m2) with
      | Some ty, Some uty -> infer_app fuel ty uty
      | _, _ -> None)

(* Computes the ratio between inference steps and size of input term (may not terminate) *)
let infer_measure ?assert_fails m =
  let fuel = ref 0 in
  let res = infer fuel [] m in
  (match (assert_fails, res) with
  | Some true, Some _ -> failwith "Expected inference to fail"
  | Some false, None -> failwith "Expected inference to succeed"
  | _ -> ());
  ( size m,
    - !fuel,
    Float.of_int (- !fuel) /. Float.of_int (size m)
    |> Float.round_decimal ~decimal_digits:2 )

(* Limits the number of inference steps to 100x size of input term (always terminates) *)
let infer_safe m =
  let size = size m in
  let fuel = ref Int.(100 * size) in
  infer fuel [] m
