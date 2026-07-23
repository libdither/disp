open Core

(* The definitions in this module are largely direct translations of Barry Jay's Rocq proofs
   at https://github.com/barry-jay-personal/combinatory-types *)

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
let fun_tag dummy = Kop * dummy
let z_tag = Kop
let product_tag = Sop * Sop
let bool_tag = Sop * Kop
let nat_tag = Sop * Kop * Kop
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
      | App (m1, m2) -> Sop * star x m1 * star x m2
      | _ -> failwith "unreachable")

let ( ^ ) = star (* for compact notation *)

let%expect_test "star" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce ("x" ^ Sop);
  [%expect {| (K S) |}];
  reduce ("x" ^ Ref "x");
  [%expect {| ((S K) K) |}];
  reduce (("a" ^ "b" ^ (Ref "b" * Ref "a")) * Ref "x" * Ref "y");
  [%expect {| (&y &x) |}]

(* Tags *)

let tag = Sop * (Sop * (Kop * Kop) * (Kop * Kop))
let tagged f t = Sop * (Sop * (Kop * Kop) * f) * (tag * (Kop * t))

let%expect_test "tagged" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (tagged (Ref "f") (Ref "t") * Ref "x");
  [%expect {| (&f &x) |}]

(* Pairing and projections *)

let ki = Kop * iop
let pair = "x" ^ "y" ^ tagged ("f" ^ (Ref "f" * Ref "x" * Ref "y")) product_tag
let fstL = Sop * iop * (Kop * Kop)
let sndL = Sop * iop * (Kop * ki)

let%expect_test "pairs" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (fstL * (pair * Ref "fst" * Ref "snd"));
  [%expect {| &fst |}];
  reduce (sndL * (pair * Ref "fst" * Ref "snd"));
  [%expect {| &snd |}]

(* Booleans *)

let tt = tagged fstL bool_tag
let ff = tagged sndL bool_tag
let cond = "b" ^ "x" ^ "y" ^ (Ref "b" * (pair * Ref "x" * Ref "y"))

let%expect_test "bool" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (cond * tt * Ref "then" * Ref "else");
  [%expect {| &then |}];
  reduce (cond * ff * Ref "then" * Ref "else");
  [%expect {| &else |}]

let of_bool = function true -> tt | false -> ff

let to_bool t =
  match sk_red (cond * t * Ref "true" * Ref "false") with
  | Ref "true" -> true
  | Ref "false" -> false
  | _ -> failwith "invalid bool"

let%expect_test "bool conversion" =
  let reduce_bool t = print_s [%sexp (to_bool t : bool)] in
  reduce_bool tt;
  [%expect {| true |}];
  reduce_bool ff;
  [%expect {| false |}];
  reduce_bool (of_bool true);
  [%expect {| true |}];
  reduce_bool (of_bool false);
  [%expect {| false |}]

let not = "b" ^ cond * Ref "b" * ff * tt

let%expect_test "not" =
  let reduce_bool t = print_s [%sexp (to_bool t : bool)] in
  reduce_bool (not * tt);
  [%expect {| false |}];
  reduce_bool (not * ff);
  [%expect {| true |}]

(* Natural numbers *)

let zero = tagged fstL nat_tag
let succ = "n" ^ tagged (Sop * sndL * (Kop * Ref "n")) nat_tag
let one = succ * zero
let rec num k = match k with 0 -> zero | n -> succ * num (n - 1)
let isZero = "n" ^ (Ref "n" * (pair * tt * (Kop * ff)))
let pred = "n" ^ (Ref "n" * (pair * zero * iop))

let%expect_test "nat" =
  print_s [%sexp (to_bool (isZero * zero) : bool)];
  [%expect {| true |}];
  print_s [%sexp (to_bool (isZero * one) : bool)];
  [%expect {| false |}];
  print_s [%sexp (to_bool (isZero * (pred * one)) : bool)];
  [%expect {| true |}]

let of_nat = num
let rec to_nat t = if to_bool (isZero * t) then 0 else 1 + to_nat (pred * t)

let%expect_test "nat conversion" =
  let reduce_nat t = print_s [%sexp (to_nat t : int)] in
  reduce_nat zero;
  [%expect {| 0 |}];
  reduce_nat one;
  [%expect {| 1 |}];
  reduce_nat (succ * one);
  [%expect {| 2 |}];
  reduce_nat (of_nat 3);
  [%expect {| 3 |}];
  reduce_nat (of_nat 42);
  [%expect {| 42 |}]

(* Sum types *)

let sum_tag = Sop * Kop * Sop
let inl_c = "p" ^ tagged (pair * tt * Ref "p") sum_tag
let inr_c = "p" ^ tagged (pair * ff * Ref "p") sum_tag

let case_c =
  "p" ^ "c"
  ^ fstL * Ref "c"
    * (pair
      * (fstL * Ref "p" * (fstL * (sndL * Ref "c")))
      * (sndL * Ref "p" * (sndL * (sndL * Ref "c"))))

let%expect_test "sum" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce
    (case_c
    * (pair * Ref "f" * Ref "g")
    * (inl_c * (pair * Ref "left" * Ref "dummy")));
  [%expect {| (&f &left) |}];
  reduce
    (case_c
    * (pair * Ref "f" * Ref "g")
    * (inr_c * (pair * Ref "dummy" * Ref "right")));
  [%expect {| (&g &right) |}]

(* Waiting *)

let wait m n = Sop * (Sop * (Kop * m) * (Kop * n)) * iop
let wop = "x" ^ "y" ^ wait (Ref "x") (Ref "y")
let wait2 m n x = Sop * (Sop * (Sop * (Kop * m) * (Kop * n)) * (Kop * x)) * iop

let%expect_test "reduction of example terms" =
  let reduce t = print_s [%sexp (sk_red t : t)] in
  reduce (wop * Ref "f" * Ref "x" * Ref "y");
  [%expect {| ((&f &x) &y) |}]

(* Function types and Fixpoints *)

let lam x m dummy = tagged (x ^ m) (Kop * dummy)

let%expect_test "lam" =
  let isZeroLam = lam "x" (isZero * Ref "x") zero in
  print_s [%sexp (to_bool (isZeroLam * zero) : bool)];
  [%expect {| true |}];
  print_s [%sexp (to_bool (isZeroLam * one) : bool)];
  [%expect {| false |}]

let cond_mono d = lam "b" (lam "x" (lam "y" (Ref "b" * (pair * Ref "x" * Ref "y")) d) d) ff

let%expect_test "cond_mono" =
  let reduce_nat t = print_s [%sexp (to_nat t : int)] in
  reduce_nat (cond_mono zero * tt * one * zero);
  [%expect {| 1 |}];
  reduce_nat (cond_mono zero * ff * one * zero);
  [%expect {| 0 |}]

let omega_z =
  "w" ^ "f" ^ "x"
  ^ Ref "f"
    * tagged
        (tagged
           (wait2 (tagged (Ref "w") wop) (tagged (Ref "w") wop) (Ref "f")) (* delay reduction of w *)
           z_tag)
        (* to make a Rec type *)
        (Kop * Ref "x")
      (* to make a function type that acts on x *)
    * Ref "x"

let z f = tagged (wait2 omega_z omega_z f) z_tag

(* Primitive Recursion *)

let primrec0_abs =
  "z" ^ "p"
  ^
  (* p : V * Nat *)
  sndL * Ref "p"
  * (pair * Ref "g"
    * ("n1"
      ^ (Ref "h" * Ref "n1" * (Ref "z" * (pair * (fstL * Ref "p") * Ref "n1")))
      ))

let primrec0 g h =
  z
    (Sop
    * (Kop * (Sop * (Sop * (Kop * sndL) * iop)))
    * (Sop
      * (Kop * (Sop * (Kop * (pair * g))))
      * (Sop
        * (Kop * (Sop * (Kop * (Sop * (Sop * (Kop * h) * iop)))))
        * (Sop
          * (Sop * (Kop * Sop)
            * (Sop * (Kop * Kop)
              * (Sop * (Kop * Sop) * (Sop * (Kop * Kop) * iop))))
          * (Kop
            * (Sop
              * (Sop * (Kop * Sop)
                * (Sop * (Kop * Kop)
                  * (Sop * (Kop * pair) * (Sop * (Kop * fstL) * iop))))
              * (Kop * iop)))))))

let primrec g h x = primrec0 (g * x) (h * x)
let prim_plus0 m n = primrec iop (Kop * (Kop * succ)) m * (pair * zero * n)
let prim_plus = "m" ^ "n" ^ prim_plus0 (Ref "m") (Ref "n")

let%expect_test "prim_plus" =
  let reduce_nat t = print_s [%sexp (to_nat t : int)] in
  reduce_nat (prim_plus * zero * zero);
  [%expect {| 0 |}];
  reduce_nat (prim_plus * one * zero);
  [%expect {| 1 |}];
  reduce_nat (prim_plus * zero * one);
  [%expect {| 1 |}];
  reduce_nat (prim_plus * of_nat 3 * of_nat 42);
  [%expect {| 45 |}];
  reduce_nat (prim_plus * of_nat 42 * of_nat 3);
  [%expect {| 45 |}]

(* Minimisation *)

let minrec_abs =
  "z" ^ "vn"
  ^ cond
    * (Ref "f" * (sndL * Ref "vn"))
    * (sndL * Ref "vn")
    * (Ref "z" * (pair * (fstL * Ref "vn") * (succ * (sndL * Ref "vn"))))

let minrec0 f =
  z
    (Sop
    * (Kop
      * (Sop
        * (Sop
          * (Sop * (Kop * cond) * (Sop * (Kop * f) * (Sop * (Kop * sndL) * iop)))
          * (Sop * (Kop * sndL) * iop))))
    * (Sop
      * (Sop * (Kop * Sop) * (Sop * (Kop * Kop) * iop))
      * (Kop
        * (Sop
          * (Sop * (Kop * pair) * (Sop * (Kop * fstL) * iop))
          * (Sop * (Kop * succ) * (Sop * (Kop * sndL) * iop))))))

let minrec f x = Sop * (Kop * minrec0 (f * x)) * (pair * zero)

let%expect_test "minrec" =
  let reduce_nat t = print_s [%sexp (to_nat t : int)] in
  reduce_nat (minrec ("_" ^ "n" ^ isZero * Ref "n") Sop * zero);
  [%expect {| 0 |}];
  reduce_nat (minrec ("_" ^ "n" ^ not * (isZero * Ref "n")) Sop * zero);
  [%expect {| 1 |}];
  reduce_nat (minrec ("_" ^ "n" ^ not * (isZero * (pred * Ref "n"))) Sop * zero);
  [%expect {| 2 |}]

(* Lists *)

let list_tag u = Sop * (Sop * Sop) * u
let nil_c = "d" ^ tagged fstL (list_tag (Ref "d"))

let cons_c =
  "p" ^ tagged ("q" ^ (sndL * Ref "q" * Ref "p")) (list_tag (fstL * Ref "p"))

let is_empty = "p" ^ (Ref "p" * (pair * tt * (Kop * ff)))

let%expect_test "list" =
  let reduce_bool t = print_s [%sexp (to_bool t : bool)] in
  let nil_nat = nil_c * zero in
  reduce_bool (is_empty * nil_nat);
  [%expect {| true |}];
  reduce_bool (is_empty * (cons_c * (pair * Ref "x" * nil_nat)));
  [%expect {| false |}];
  reduce_bool
    (is_empty
    * (cons_c * (pair * Ref "x" * (cons_c * (pair * Ref "y" * nil_nat)))));
  [%expect {| false |}]

let rec of_list dummy = function
  | [] -> nil_c * dummy
  | x :: xs -> cons_c * (pair * x * of_list x xs)

let rec to_list t =
  match sk_red (t * (pair * Ref "nil" * ("p" ^ (Ref "p" * Ref "cons")))) with
  | Ref "nil" -> []
  | App (App (Ref "cons", hd), tl) -> hd :: to_list tl
  | _ -> failwith "invalid list"

let%expect_test "list conversion" =
  let reduce_list t =
    print_s [%sexp (to_list t |> List.map ~f:to_nat : int list)]
  in
  let nil_nat = nil_c * zero in
  reduce_list nil_nat;
  [%expect {| () |}];
  reduce_list (cons_c * (pair * one * nil_nat));
  [%expect {| (1) |}];
  reduce_list (cons_c * (pair * zero * (cons_c * (pair * one * nil_nat))));
  [%expect {| (0 1) |}];
  reduce_list (of_list zero []);
  [%expect {| () |}];
  reduce_list (of_list zero [ of_nat 7; of_nat 8; of_nat 9 ]);
  [%expect {| (7 8 9) |}]

let fold_left_c_abs =
  "z" ^ "p"
  ^ sndL * Ref "p"
    * (pair * (fstL * Ref "p")
      * ("q"
        ^ Ref "z"
          * (pair
            * (Ref "f" * (fstL * Ref "p") * (fstL * Ref "q"))
            * (sndL * Ref "q"))))

let fold_left_c f =
  z
    (Sop
    * (Kop * (Sop * (Sop * (Kop * sndL) * iop)))
    * (Sop
      * (Kop * (Sop * (Sop * (Kop * pair) * (Sop * (Kop * fstL) * iop))))
      * (Sop
        * (Sop * (Kop * Sop)
          * (Sop * (Kop * Kop) * (Sop * (Kop * Sop) * (Sop * (Kop * Kop) * iop)))
          )
        * (Kop
          * (Sop
            * (Sop * (Kop * Sop)
              * (Sop
                * (Kop * (Sop * (Kop * pair)))
                * (Sop
                  * (Sop * (Kop * Sop)
                    * (Sop * (Kop * Kop)
                      * (Sop * (Kop * f) * (Sop * (Kop * fstL) * iop))))
                  * (Kop * (Sop * (Kop * fstL) * iop)))))
            * (Kop * (Sop * (Kop * sndL) * iop)))))))

let%expect_test "fold_left_c" =
  let sum list =
    let list = of_list zero (List.map ~f:of_nat list) in
    let t = fold_left_c prim_plus * (pair * zero * list) in
    print_s [%sexp (to_nat t : int)]
  in
  sum [];
  [%expect {| 0 |}];
  sum [ 42 ];
  [%expect {| 42 |}];
  sum [ 0; 1; 2 ];
  [%expect {| 3 |}];
  sum [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 0 ];
  [%expect {| 45 |}]
