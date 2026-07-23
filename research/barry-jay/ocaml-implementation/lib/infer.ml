open Core
open Sk
open Types

(* The definitions in this module are largely direct translations of Barry Jay's Rocq proofs
   at https://github.com/barry-jay-personal/combinatory-types *)

let rec infer_app fuel ty uty =
  Int.decr fuel;
  if !fuel = 0 then None
  else
    match ty with
    (* combinatory types *)
    | Kty -> Some (K1ty uty)
    | K1ty uty1 -> Some uty1
    | Sty -> Some (S1ty uty)
    | S1ty uty1 -> (
        let rty = S2ty (uty1, uty) in
        match unpack_constructor rty with
        | None -> Some rty (* not the form of a constructor *)
        | Some (fty, tty) -> (
            if
              (* may be a constructor *)

              (* true *)
              equal_ty fty (p_type fstL) && equal_ty tty (S1ty Kty)
            then Some bool
            else if
              (* false *)
              equal_ty fty (p_type sndL) && equal_ty tty (S1ty Kty)
            then Some bool
            else if
              (* zero *)
              equal_ty fty (p_type fstL) && equal_ty tty ity
            then Some nat
            else if
              (* a successor *)
              equal_ty fty (S2ty (p_type sndL, K1ty nat)) && equal_ty tty ity
            then Some nat
            else
              match tty with
              | S1ty Sty -> (
                  (* a pair *)
                  match fty with
                  | S2ty (S2ty (S2ty (Kty, Kty), K1ty uty3), K1ty vty3) ->
                      Some (product uty3 vty3)
                  | _ -> None)
              | S2ty (Kty, Sty) -> (
                  (* a sum *)
                  match fty with
                  | Data2
                      (S1ty Sty, Data0 (S1ty Kty), Data2 (S1ty Sty, uty2, vty2))
                    ->
                      Some (sum uty2 vty2)
                  | _ -> None)
              | K1ty uty2 -> (
                  (* a function *)
                  match infer_app fuel fty uty2 with
                  | Some vty -> Some (funty uty2 vty)
                  | _ -> None)
              | S2ty (S1ty Sty, uty1) -> (
                  if
                    (* a list *)
                    equal_ty fty (p_type fstL)
                  then Some (list uty1)
                  else
                    match fty with
                    | S2ty
                        ( S2ty
                            ( K1ty
                                (S2ty
                                   ( S2ty (Kty, Kty),
                                     K1ty (K1ty (S2ty (Kty, Kty))) )),
                              S2ty (Kty, Kty) ),
                          K1ty (Data2 (S1ty Sty, uty2, uty3)) ) ->
                        if equal_ty uty3 (list uty2) && equal_ty uty2 uty1 then
                          Some (list uty1)
                        else None
                    | _ -> None)
              | _ -> (
                  match unpack_Zty fty with
                  | Some fty1 ->
                      (* a Z *)
                      if equal_ty tty Kty then Some (rec_ fty1) else None
                  | _ ->
                      (* unknown constructor *)
                      None)))
    | S2ty (uty1, vty1) ->
        let%bind.Option vty2 = infer_app fuel vty1 uty in
        let%bind.Option ty2 = infer_app fuel uty1 uty in
        let%bind.Option wty = infer_app fuel ty2 vty2 in
        Some wty
    (* product types *)
    | Data2 (S1ty Sty, ty1, ty2) -> (
        match infer_app fuel uty ty1 with
        | None -> None
        | Some vty2 -> infer_app fuel vty2 ty2)
    (* Bool *)
    | Data0 (S1ty Kty) -> (
        match uty with
        | Data2 (S1ty Sty, uty1, vty1) ->
            (* Product uty1 vty1 *)
            if equal_ty uty1 vty1 then Some uty1 else None
        | _ -> None)
    (* Nat *)
    | Data0 (S2ty (Kty, Kty)) -> (
        match uty with
        | Data2 (S1ty Sty, uty1, vty1) -> (
            match infer_app fuel vty1 nat with
            | Some uty2 -> if equal_ty uty1 uty2 then Some uty1 else None
            | _ -> None)
        | _ -> None)
    (* sum types *)
    | Data2 (S2ty (Kty, Sty), uty1, vty1) ->
        infer_app fuel (product bool (product uty1 vty1)) uty
    (* function types *)
    | Data2 (K1ty Kty, uty1, vty1) ->
        if equal_ty uty uty1 then Some vty1 else None
    (* recursive types *)
    | Data1 (Kty, fty) -> (
        match uty with
        | Data2 (S1ty Sty, vty, uty1) -> (
            (* Product vty uty1 *)
            match
              infer_app fuel fty
                (Data2 (K1ty Kty, Data2 (S1ty Sty, vty, uty1), vty))
            with
            (* V1 * U1 -> V2 *)
            | None -> None
            | Some ty2 -> (
                match infer_app fuel ty2 uty with
                | None -> None
                | Some vty2 -> if equal_ty vty2 vty then Some vty else None))
        | _ -> None)
    (* list types *)
    | Data1 (S2ty (S1ty Sty, Sty), uty1) -> (
        match uty with
        | Data2 (S1ty Sty, vty1, vty2) -> (
            (* a product *)
            match infer_app fuel vty2 (product uty1 (list uty1)) with
            | Some vty3 -> if equal_ty vty3 vty1 then Some vty1 else None
            | _ -> None)
        | _ -> None)
    | _ -> None

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
