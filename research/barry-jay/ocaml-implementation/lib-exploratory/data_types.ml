open Core
open Sk
open Types

(* This module defines abstract data types using the [Data_type] module.
   Note that [Data_type.create] not only emits (tagged) constructors, but
   also internally registeres all the machinery required to introduce and
   eliminate instances of the type. *)

let to_tup0_exn = function [] -> () | _ -> failwith "wrong size"
let to_tup1_exn = function [ t1 ] -> t1 | _ -> failwith "wrong size"
let to_tup2_exn = function [ t1; t2 ] -> (t1, t2) | _ -> failwith "wrong size"
let at_exn i l = List.nth_exn l i

(* Pairing and projections *)

let product, pair =
  Data_type.create ~friendly_name:"product" ~type_args:2
    ~ctors:(fun _product_tag ->
      [
        ( [ `Term ("u", at_exn 0); `Term ("v", at_exn 1) ],
          "f" ^ (Ref "f" * Ref "u" * Ref "v"),
          fun ~app:_ _tag_args args -> Some args );
      ])
  |> fun (tag, ctors) -> (tag, to_tup1_exn ctors)

let%expect_test "pairs" =
  let reduce t = print_s [%sexp (sk_red t : Sk.t)] in
  reduce (fst * (pair * Ref "fst" * Ref "snd"));
  [%expect {| &fst |}];
  reduce (snd * (pair * Ref "fst" * Ref "snd"));
  [%expect {| &snd |}]

(* Function types and Fixpoints *)

let fun_, to_fun =
  Data_type.create_manual ~friendly_name:"fun" ~type_args:2
    ~ctors:(fun _fun_tag ->
      [
        ( [ `Term "f"; `Type ("arg_ty", 0) ],
          Ref "f",
          fun ~app tag_args args ->
            match (tag_args, args) with
            | [ Some uty; _ ], [ f ] ->
                let%bind.Option rty = app f uty in
                Some [ uty; rty ]
            | _ -> None );
      ])
    ~app:(fun ~app:_ ~ty_args ~arg ->
      match ty_args with
      | [ uty1; vty1 ] when equal_ty arg uty1 -> Some vty1
      | _ -> None)
  |> fun (tag, ctors) -> (tag, to_tup1_exn ctors)

let lam x m ~arg = sk_red (to_fun * (x ^ m) * arg)

let%expect_test "lam" =
  let id_lam = lam "x" (Ref "x") ~arg:(Tag.sk_of product) in
  print_s [%sexp (sk_red (id_lam * Kop) : Sk.t)];
  [%expect {| K |}];
  print_s [%sexp (sk_red (id_lam * Sop) : Sk.t)];
  [%expect {| S |}];
  let id_lam = lam "x" (Ref "x") ~arg:(Tag.dummy_value Kop) in
  print_s [%sexp (sk_red (id_lam * Kop) : Sk.t)];
  [%expect {| K |}];
  print_s [%sexp (sk_red (id_lam * Sop) : Sk.t)];
  [%expect {| S |}]

let id = "type" ^ (to_fun * iop * Ref "type")

(* Booleans *)

let bool, (tt, ff) =
  Data_type.create ~friendly_name:"bool" ~type_args:0 ~ctors:(fun _bool_tag ->
      [
        ( [],
          "t" ^ lam "e" (Ref "t") ~arg:(Tag.dummy_value (Ref "t")),
          fun ~app:_ _tag_args _args -> Some [] );
        ( [],
          "t" ^ lam "e" (Ref "e") ~arg:(Tag.dummy_value (Ref "t")),
          fun ~app:_ _tag_args _args -> Some [] );
      ])
  |> fun (tag, ctors) -> (tag, to_tup2_exn ctors)

let cond = "b" ^ "x" ^ "y" ^ (Ref "b" * Ref "x" * Ref "y")
let not_ = "b" ^ (cond * Ref "b" * ff * tt)

let%expect_test "bool" =
  let reduce t = print_s [%sexp (sk_red t : Sk.t)] in
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

(* Natural numbers *)

let nat, (zero, succ) =
  Data_type.create ~friendly_name:"nat" ~type_args:0 ~ctors:(fun nat_tag ->
      [
        ( [],
          "z"
          ^ lam "s" (Ref "z")
              ~arg:
                (Tag.sk_of fun_
                   ~args:
                     [
                       Some (Tag.sk_of nat_tag);
                       Some (Tag.dummy_value (Ref "z"));
                     ]),
          fun ~app:_ _tag_args _args -> Some [] );
        ( [ `Term ("n", fun _ -> Tag.data_type_of nat_tag []) ],
          "z"
          ^ lam "s" (Ref "s" * Ref "n")
              ~arg:
                (Tag.sk_of fun_
                   ~args:
                     [
                       Some (Tag.sk_of nat_tag);
                       Some (Tag.dummy_value (Ref "z"));
                     ]),
          fun ~app:_ _tag_args args ->
            match args with
            | [ n ] when equal_ty n (Tag.data_type_of nat_tag []) -> Some []
            | _ -> None );
      ])
  |> fun (tag, ctors) -> (tag, to_tup2_exn ctors)

let one = succ * zero
let rec num k = match k with 0 -> zero | n -> succ * num (n - 1)
let isZero = "n" ^ (Ref "n" * tt * lam "n" ff ~arg:(Tag.sk_of nat))
let pred = "n" ^ (Ref "n" * zero * lam "n" (Ref "n") ~arg:(Tag.sk_of nat))

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

let sum, (inl, inr) =
  Data_type.create ~friendly_name:"sum" ~type_args:2 ~ctors:(fun _sum_tag ->
      [
        ( [ `Term ("l", fun args -> List.nth_exn args 0); `Type ("r", 1) ],
          "resty"
          ^ lam "onl"
              (lam "onr" (Ref "onl" * Ref "l")
                 ~arg:
                   (Tag.sk_of fun_ ~args:[ Some (Ref "r"); Some (Ref "resty") ]))
              ~arg:(Tag.sk_of fun_ ~args:[ Some (Ref "l"); Some (Ref "resty") ]),
          fun ~app:_ tag_args args ->
            match tag_args with
            | [ _; Some r ] -> Some (args @ [ r ])
            | _ -> None );
        ( [ `Type ("l", 0); `Term ("r", fun args -> List.nth_exn args 1) ],
          "resty"
          ^ lam "onl"
              (lam "onr" (Ref "onr" * Ref "r")
                 ~arg:
                   (Tag.sk_of fun_ ~args:[ Some (Ref "r"); Some (Ref "resty") ]))
              ~arg:(Tag.sk_of fun_ ~args:[ Some (Ref "l"); Some (Ref "resty") ]),
          fun ~app:_ tag_args args ->
            match tag_args with
            | [ Some l; _ ] -> Some ([ l ] @ args)
            | _ -> None );
      ])
  |> fun (tag, ctors) -> (tag, to_tup2_exn ctors)

let case_c =
  "resty" ^ "onl" ^ "onr" ^ "c" ^ (Ref "c" * Ref "resty" * Ref "onl" * Ref "onr")

let%expect_test "sum" =
  let reduce t = print_s [%sexp (sk_red t : Sk.t)] in
  reduce
    (case_c * Ref "ty" * Ref "f" * Ref "g"
    * (inl * Ref "left" * Tag.dummy_value (Ref "dummy")));
  [%expect {| (&f &left) |}];
  reduce
    (case_c * Ref "ty" * Ref "f" * Ref "g"
    * (inr * Tag.dummy_value (Ref "dummy") * Ref "right"));
  [%expect {| (&g &right) |}]

(* Recursion *)

let rec_, z =
  Data_type.create_manual ~friendly_name:"z" ~type_args:1
    ~ctors:(fun z_tag ->
      [
        ( [ `Term "f" ],
          (let omega_z =
             "w" ^ "f" ^ "x"
             ^ Ref "f"
               * (to_fun
                 * tagged
                     (wait2 (Ref "w") (Ref "w") (Ref "f"))
                     (* delay reduction of w *)
                     (Tag.sk_of z_tag |> tag)
                 (* to make a Rec type *)
                 * Tag.dummy_value (Ref "x"))
                 (* to make a function type that acts on x *)
               * Ref "x"
           in
           wait2 omega_z omega_z (Ref "f")),
          fun ~app:_ _tag_args args -> Some args );
      ])
    ~app:(fun ~app ~ty_args ~arg ->
      match (ty_args, arg) with
      | [ fty ], Data { ctr; args = [ vty; uty1 ] }
        when Int.equal ctr product.ctr ->
          (* V1 * U1 -> V2 *)
          let%bind.Option ty2 =
            app fty
              (Tag.data_type_of fun_
                 [ Tag.data_type_of product [ vty; uty1 ]; vty ])
          in
          let%bind.Option vty2 = app ty2 arg in
          if equal_ty vty2 vty then Some vty else None
      | [ fty ], vty ->
          (* V1 -> V2 *)
          let%bind.Option ty2 = app fty (Tag.data_type_of fun_ [ vty; vty ]) in
          let%bind.Option vty2 = app ty2 arg in
          if equal_ty vty2 vty then Some vty else None
      | _ ->
          print_s [%sexp (ty_args : t list)];
          None)
  |> fun (tag, ctors) -> (tag, to_tup1_exn ctors)

(* Primitive Recursion *)

let primrec0 g h =
  z
  * ("z" ^ "p"
    ^ snd * Ref "p" * g
      * lam "n1"
          (h * Ref "n1" * (Ref "z" * (pair * (fst * Ref "p") * Ref "n1")))
          ~arg:(Tag.sk_of nat))

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

let minrec0 f =
  z
  * ("z" ^ "vn"
    ^ cond
      * (f * (snd * Ref "vn"))
      * (snd * Ref "vn")
      * (Ref "z" * (pair * (fst * Ref "vn") * (succ * (snd * Ref "vn")))))

let minrec f x = Sop * (Kop * minrec0 (f * x)) * (pair * zero)

(* Lists *)

let list, (nil, cons) =
  Data_type.create ~friendly_name:"list" ~type_args:1 ~ctors:(fun list_tag ->
      [
        ( [ `Type ("elem_ty", 0) ],
          fst,
          fun ~app:_ tag_args _args ->
            match tag_args with [ Some uty ] -> Some [ uty ] | _ -> None );
        ( [
            `Term
              ( "p",
                fun args ->
                  let ty = List.nth_exn args 0 in
                  Tag.data_type_of product
                    [ ty; Tag.data_type_of list_tag [ ty ] ] );
          ],
          "q" ^ (snd * Ref "q" * Ref "p"),
          fun ~app:_ tag_args args ->
            match (tag_args, args) with
            | ( [ ety1 ],
                [
                  Data
                    {
                      ctr = prodctr;
                      args = [ ety2; Data { ctr = listctr; args = [ ety3 ] } ];
                    };
                ] )
            (* Some uty *)
              when Int.equal prodctr product.ctr
                   && Int.equal listctr list_tag.ctr
                   && equal_ty ety2 ety3
                   &&
                   match ety1 with
                   | None -> true
                   | Some ety1 -> equal_ty ety1 ety2 ->
                Some [ ety3 ]
            | _ -> None );
      ])
  |> fun (tag, ctors) -> (tag, to_tup2_exn ctors)

let is_empty = "p" ^ (Ref "p" * (pair * tt * (Kop * ff)))

let%expect_test "list" =
  let reduce_bool t = print_s [%sexp (to_bool t : bool)] in
  let nil_nat = nil * Tag.dummy_value zero in
  reduce_bool (is_empty * nil_nat);
  [%expect {| true |}];
  reduce_bool (is_empty * (cons * (pair * Ref "x" * nil_nat)));
  [%expect {| false |}];
  reduce_bool
    (is_empty * (cons * (pair * Ref "x" * (cons * (pair * Ref "y" * nil_nat)))));
  [%expect {| false |}]

let rec of_list dummy = function
  | [] -> nil * Tag.dummy_value dummy
  | x :: xs -> cons * (pair * x * of_list x xs)

let rec to_list t =
  match sk_red (t * (pair * Ref "nil" * ("p" ^ (Ref "p" * Ref "cons")))) with
  | Ref "nil" -> []
  | App (App (Ref "cons", hd), tl) -> hd :: to_list tl
  | _ -> failwith "invalid list"

let%expect_test "list conversion" =
  let reduce_list t =
    print_s [%sexp (to_list t |> List.map ~f:to_nat : int list)]
  in
  let nil_nat = nil * Tag.dummy_value zero in
  reduce_list nil_nat;
  [%expect {| () |}];
  reduce_list (cons * (pair * one * nil_nat));
  [%expect {| (1) |}];
  reduce_list (cons * (pair * zero * (cons * (pair * one * nil_nat))));
  [%expect {| (0 1) |}];
  reduce_list (of_list zero []);
  [%expect {| () |}];
  reduce_list (of_list zero [ of_nat 7; of_nat 8; of_nat 9 ]);
  [%expect {| (7 8 9) |}]

let fold_left f =
  z
  * ("z" ^ "p"
    ^ snd * Ref "p"
      * (pair * (fst * Ref "p")
        * ("q"
          ^ Ref "z"
            * (pair * (f * (fst * Ref "p") * (fst * Ref "q")) * (snd * Ref "q"))
          )))

let%expect_test "fold_left" =
  let sum list =
    let list = List.map ~f:of_nat list |> of_list zero in
    let t = fold_left prim_plus * (pair * zero * list) in
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

let empty_of =
  z
  * ("z" ^ "l"
    ^ (Ref "l" * (pair * Ref "l" * ("q" ^ (Ref "z" * (snd * Ref "q"))))))

let reverse =
  "l"
  ^ fold_left ("acc" ^ "x" ^ (cons * (pair * Ref "x" * Ref "acc")))
    * (pair * (empty_of * Ref "l") * Ref "l")

let%expect_test "reverse" =
  let reverse list =
    let list = List.map ~f:of_nat list |> of_list zero in
    let t = reverse * list in
    print_s [%sexp (to_list t |> List.map ~f:to_nat : int list)]
  in
  reverse [];
  [%expect {| () |}];
  reverse [ 42 ];
  [%expect {| (42) |}];
  reverse [ 0; 1; 2 ];
  [%expect {| (2 1 0) |}];
  reverse [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 0 ];
  [%expect {| (0 1 2 3 4 5 6 7 8 9) |}]

let fold_right f =
  "p"
  ^ fold_left ("acc" ^ "x" ^ (f * Ref "x" * Ref "acc"))
    * (pair * (fst * Ref "p") * (reverse * (snd * Ref "p")))

let%expect_test "fold_right" =
  let sum list =
    let list = List.map ~f:of_nat list |> of_list zero in
    let t = fold_right prim_plus * (pair * zero * list) in
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

let map elem_type =
  "f" ^ "l"
  ^ fold_right ("x" ^ "tl" ^ (cons * (pair * (Ref "f" * Ref "x") * Ref "tl")))
    * (pair * (nil * elem_type) * Ref "l")

let%expect_test "map" =
  let map list =
    let list = List.map ~f:of_nat list |> of_list zero in
    let t = map (Tag.sk_of nat) * succ * list in
    print_s [%sexp (to_list t |> List.map ~f:to_nat : int list)]
  in
  map [];
  [%expect {| () |}];
  map [ 42 ];
  [%expect {| (43) |}];
  map [ 0; 1; 2 ];
  [%expect {| (1 2 3) |}];
  map [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 0 ];
  [%expect {| (10 9 8 7 6 5 4 3 2 1) |}]

let list_module elem_type =
  let list elem_type = Tag.sk_of ~args:[ Some elem_type ] list in
  let product u_type v_type =
    Tag.sk_of ~args:[ Some u_type; Some v_type ] product
  in
  pair
  * (pair * (nil * elem_type)
    * (to_fun * cons * product elem_type (list elem_type)))
  * (to_fun * reverse * list elem_type)
