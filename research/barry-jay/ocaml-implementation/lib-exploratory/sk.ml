open Core

(* Mostly the same as the [lib] version, but abstracting the notion of tags,
   which will be useful to create both type-level and term-level encodings of tags. *)

type t =
  | Ref of string (* variables are indexed by strings *)
  | Sop
  | Kop
  | App of (t * t)
[@@deriving equal]

let rec sexp_of_t = function
  | Ref x -> Sexp.Atom ("&" ^ x)
  | Sop -> Atom "S"
  | Kop -> Atom "K"
  | App (x, y) -> [%sexp (x : t), (y : t)]

let rec size t =
  match t with Ref _ | Sop | Kop -> 1 | App (x, y) -> size x + size y

let ( * ) x y = App (x, y) (* for compact notation *)
let iop = Sop * Kop * Kop

(* SK-reduction *)

let rec sk_red_at z =
  match z with
  | Ref _ | Sop | Kop -> z
  | App (y, z) -> (
      let y = sk_red_at y in
      match y with
      | Ref _ | Sop | Kop -> y * z
      | App (x, y) -> (
          match x with
          | Ref _ | Sop -> x * y * z
          | Kop -> sk_red_at y
          | App (w, x) -> (
              match w with
              | Ref _ | Kop -> w * x * y * z
              | Sop -> sk_red_at (x * z * (y * z))
              | App (v, w) -> v * w * x * y * z)))

let rec sk_red sk =
  let sk = sk_red_at sk in
  match sk with Ref _ | Sop | Kop -> sk | App (m, n) -> sk_red m * sk_red n

let%expect_test "sk_red" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce Kop;
  [%expect {| K |}];
  reduce (Kop * Ref "a" * Ref "b");
  [%expect {| &a |}];
  reduce (Sop * Ref "a" * Ref "b" * Ref "c");
  [%expect {| ((&a &c) (&b &c)) |}];
  reduce (iop * Ref "x");
  [%expect {| &x |}]

(* Tags *)

module Tag = struct
  type 'arg t = { ctr : int; args : 'arg list } [@@deriving equal, sexp_of]
end

let tag t = Sop * (Sop * (Kop * Kop) * (Kop * Kop)) * t
let tagged f t = Sop * (Sop * (Kop * Kop) * f) * t

let%expect_test "tagged" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (tagged (Ref "f") (Ref "g") * Ref "x");
  [%expect {| (&f &x) |}]

(* star abstraction *)

let rec occurs x m =
  match m with
  | Ref y -> [%equal: string] x y
  | App (m1, m2) -> occurs x m1 || occurs x m2
  | _ -> false

let rec star x m =
  match occurs x m with
  | false -> Kop * m
  | true -> (
      match m with
      | Ref _ -> iop
      | App (m1, Ref x2) when String.equal x x2 && not (occurs x m1) ->
          (* TODO: Explicitly exclude [tagged f t] values? *)
          m1
      | App (m1, m2) -> Sop * star x m1 * star x m2
      | _ -> failwith "unreachable")

let ( ^ ) = star (* for compact notation *)

let%expect_test "star" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce ("x" ^ Sop);
  [%expect {| (K S) |}];
  reduce ("x" ^ Ref "x");
  [%expect {| ((S K) K) |}];
  reduce ("x" ^ (Ref "y" * Ref "x"));
  [%expect {| &y |}];
  reduce (("a" ^ "b" ^ (Ref "b" * Ref "a")) * Ref "x" * Ref "y");
  [%expect {| (&y &x) |}]

let ki = Kop * iop
let fst = Sop * iop * (Kop * Kop)
let snd = Sop * iop * (Kop * ki)

(* Waiting *)

let wait m n = Sop * (Sop * (Kop * m) * (Kop * n)) * iop
let wop = "x" ^ "y" ^ wait (Ref "x") (Ref "y")
let wait2 m n x = Sop * (Sop * (Sop * (Kop * m) * (Kop * n)) * (Kop * x)) * iop

let%expect_test "reduction of example terms" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (wop * Ref "f" * Ref "x" * Ref "y");
  [%expect {| ((&f &x) &y) |}]
