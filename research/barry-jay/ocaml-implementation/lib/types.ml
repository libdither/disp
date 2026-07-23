open Core
open Sk

(* The definitions in this module are largely direct translations of Barry Jay's Rocq proofs
   at https://github.com/barry-jay-personal/combinatory-types *)

type t =
  | Kty
  | K1ty of t
  | Sty
  | S1ty of t
  | S2ty of t * t
  (* the 1st arg is the type of a tag *)
  | Data0 of t
  | Data1 of t * t
  | Data2 of t * t * t
[@@deriving equal, sexp_of]

let equal_ty = [%equal: t]

let rec get x ps =
  match ps with
  | [] -> None
  | (y, uty) :: ps1 -> if String.equal y x then Some uty else get x ps1

let rec pc_type gamma p =
  match p with
  | Sop -> Sty
  | Kop -> Kty
  | App (Sop, p1) -> S1ty (pc_type gamma p1)
  | App (Kop, p1) -> K1ty (pc_type gamma p1)
  | App (App (Sop, p1), p2) -> S2ty (pc_type gamma p1, pc_type gamma p2)
  | Ref x -> (
      match get x gamma with Some ty -> ty | _ -> failwith "invalid program")
  | _ -> failwith "invalid program"

let p_type p = pc_type [] p

let%expect_test "p_type" =
  let p_type sk =
    print_s (try [%sexp (p_type sk : t)] with _ -> Atom "failed")
  in
  p_type Kop;
  [%expect {| Kty |}];
  p_type (Kop * Kop);
  [%expect {| (K1ty Kty) |}];
  p_type (Kop * Kop * Kop);
  [%expect {| failed |}];
  p_type iop;
  [%expect {| (S2ty Kty Kty) |}]

let product_tag_ty = p_type product_tag
let product tty fty = Data2 (product_tag_ty, tty, fty)
let bool_tag_ty = p_type bool_tag
let bool = Data0 bool_tag_ty
let nat_tag_ty = p_type nat_tag
let nat = Data0 nat_tag_ty
let sum_tag_ty = p_type sum_tag
let sum tty fty = Data2 (sum_tag_ty, tty, fty)
let fun_tag_ty = p_type (fun_tag Kop)
let funty uty vty = Data2 (fun_tag_ty, uty, vty)
let z_tag_ty = p_type z_tag
let rec_ fty = Data1 (z_tag_ty, fty)
let list_tag_ty = p_type (list_tag Sop)
let list ty = Data1 (list_tag_ty, ty)

let known =
  [
    ("product", product_tag_ty);
    ("bool", bool_tag_ty);
    ("nat", nat_tag_ty);
    ("sum", sum_tag_ty);
    ("fun", fun_tag_ty);
    ("z", z_tag_ty);
    ("list", list_tag_ty);
  ]

let ity = p_type iop

(* like what ppx_sexp_conv generates, but with human-friendly representation for known data types *)
let rec sexp_of_t t =
  let known_or alt tag args =
    match List.find known ~f:(fun (_, ty) -> equal_ty tag ty) with
    | Some (name, _) ->
        if List.is_empty args then Sexp.Atom name else List (Atom name :: args)
    | None -> List (Sexp.Atom alt :: sexp_of_t tag :: args)
  in
  match t with
  | Kty -> Sexp.Atom "Kty"
  | K1ty ty -> List [ Atom "K1ty"; sexp_of_t ty ]
  | Sty -> Sexp.Atom "Sty"
  | S1ty vty -> List [ Atom "S1ty"; sexp_of_t vty ]
  | S2ty (uty, vty) -> List [ Atom "S2ty"; sexp_of_t uty; sexp_of_t vty ]
  | Data0 tag -> known_or "Data0" tag []
  | Data1 (tag, uty) -> known_or "Data1" tag [ sexp_of_t uty ]
  | Data2 (tag, uty, vty) ->
      known_or "Data2" tag [ sexp_of_t uty; sexp_of_t vty ]

let omega_z_ty = p_type omega_z

let unpack_constructor ty =
  match ty with
  | S2ty (S2ty (K1ty Kty, fty), S2ty (S2ty (K1ty Kty, K1ty Kty), K1ty tty)) ->
      Some (fty, tty)
  | _ -> None

let unpack_wait2 ty =
  match ty with
  | S2ty (S2ty (S2ty (K1ty ty1, K1ty ty2), K1ty ty3), S2ty (Kty, Kty)) ->
      Some (ty1, ty2, ty3)
  | _ -> None

let unpack_Zty ty =
  match unpack_wait2 ty with
  | Some (ty1, ty2, ty3) ->
      if equal_ty ty1 omega_z_ty && equal_ty ty2 omega_z_ty then Some ty3
      else None
  | _ -> None
