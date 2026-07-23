open Core
open Sk

(* Compared to the [lib] version of this module, abstract data types
   have been generalized. Tag arguments now directly mirror type arguments,
   but are generally optional. For instance, a pair could be tagged with
   tags [product None None] or [product (Some u) None] etc while an empty
   list [nil] must be tagged with [list (Some e)]. Further, tag arguments
   are now not limited to dummy values, but can themselves be tags, e.g.
   [u] and [e] above could be either a dummy value like [ff] or a tag like [bool]. *)

type t = Kty | K1ty of t | Sty | S1ty of t | S2ty of t * t | Data of data
and data = t Tag.t [@@deriving equal, sexp_of]

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
      match get x gamma with
      | Some ty -> ty
      | None -> raise_s [%sexp "invalid program, undefined ref:", (x : string)])
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

(* Matches a term with holes (variables) against a combinatory type.
   The type fragments that fall into holes need not be combinatory,
   and are returned as a mapping from variable names to types. *)
let p_type_match pattern ty =
  let res_empty = Map.empty (module String) in
  let res_merge =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (a, b) -> if equal_ty a b then Some a else None
      | `Left v | `Right v -> Some v)
  in
  let rec p_type_match pattern ty =
    match (ty, pattern) with
    | Sty, Sop -> Some res_empty
    | Kty, Kop -> Some res_empty
    | S1ty ty1, App (Sop, p1) ->
        let%bind.Option res1 = p_type_match p1 ty1 in
        Some res1
    | K1ty ty1, App (Kop, p1) ->
        let%bind.Option res1 = p_type_match p1 ty1 in
        Some res1
    | S2ty (ty1, ty2), App (App (Sop, p1), p2) ->
        let%bind.Option res1 = p_type_match p1 ty1 in
        let%bind.Option res2 = p_type_match p2 ty2 in
        Some (res_merge res1 res2)
    | ty, Ref x -> Some (Map.set res_empty ~key:x ~data:ty)
    | _ -> None
  in
  p_type_match pattern ty

(* let p_type_match pattern ty = p_type_match (sk_red pattern) ty *)

(* Encoding and decoding tags *)

module Tag = struct
  type ty = t [@@deriving sexp_of]

  include Tag

  (* Tags are either dummy values or "proper".
     Only proper tags are allowed as arguments to [tagged],
     after all if the value being tagged was sufficient to
     determine the type, we wouldn't need to tag it.

     tag =
       | K (ctr : int, args : tag list)
       | S dummy
  *)

  let dummy_value t = Sop * t

  let sk_of =
    let t_of_bool = function true -> Sop | false -> Kop in

    let rec t_of_list ~t_of = function
      | [] -> Kop
      | x :: xs -> Sop * t_of x * t_of_list ~t_of xs
    in
    let rec bits_of_nat n =
      if n = 0 then [] else (n mod 2 > 0) :: bits_of_nat (n / 2)
    in
    let t_of_nat n = bits_of_nat n |> t_of_list ~t_of:t_of_bool in
    let t_of_pair x y = Sop * x * y in
    let t_of_option ~t_of = function None -> Kop | Some x -> Sop * t_of x in
    fun ?args { Tag.ctr; args = tag_args } ->
      let ctr = t_of_nat ctr in
      let args =
        match args with
        | Some args -> List.map2_exn args tag_args ~f:Option.first_some
        | None -> tag_args
      in
      let args = t_of_list ~t_of:(t_of_option ~t_of:Fn.id) args in
      Kop * t_of_pair ctr args

  let of_combinatory_type =
    let bool_of_t = function
      | Sty -> true
      | Kty -> false
      | _ -> failwith "invalid aux bool"
    in
    let rec list_of_t ~of_t t =
      match t with
      | Kty -> []
      | S2ty (x, xs) -> of_t x :: list_of_t ~of_t xs
      | _ -> failwith "invalid aux list"
    in
    let rec nat_of_bits = function
      | [] -> 0
      | x :: xs -> Int.((if x then 1 else 0) + (2 * nat_of_bits xs))
    in
    let nat_of_t t = list_of_t ~of_t:bool_of_t t |> nat_of_bits in
    let pair_of_t t =
      match t with S2ty (x, y) -> (x, y) | _ -> failwith "invalid aux pair"
    in
    let option_of_t ~of_t t =
      match t with
      | Kty -> None
      | S1ty x -> Some (of_t x)
      | _ -> failwith "invalid aux option"
    in
    let rec tag_of_t t =
      let ctr, args = pair_of_t t in
      let ctr = nat_of_t ctr in
      let args = list_of_t ~of_t:(option_of_t ~of_t:aux) args in
      { Tag.ctr; args }
    and aux = function
      | K1ty t ->
          let { Tag.ctr; args } = tag_of_t t in
          Data
            {
              ctr;
              args =
                List.map
                  ~f:(fun arg ->
                    Option.value_exn
                      ~message:
                        (Sexp.to_string
                           [%sexp
                             "tag arg was None",
                             ~~(ctr : int),
                             ~~(args : ty option list)])
                      arg)
                  args;
            }
      | S1ty t -> t
      | Data { ctr; args } -> Data { ctr; args }
      | other -> raise_s [%sexp "invalid tag", (other : ty)]
    in
    fun t -> tag_of_t t

  let data_type_of t args =
    if Int.equal (List.length t.Tag.args) (List.length args) then
      Data { ctr = t.Tag.ctr; args }
    else raise_s [%sexp "type arg count mismatch"]
end

(* Abstract data types *)

module Data_type : sig
  type app := t -> t -> t option

  val create :
    friendly_name:string ->
    type_args:int ->
    ctors:
      (Sk.t option Tag.t ->
      ([ `Term of string * (t list -> t) | `Type of string * int ] list
      * Sk.t
      * (app:app -> t option list -> t list -> t list option))
      list) ->
    Sk.t option Tag.t * Sk.t list

  val create_manual :
    friendly_name:string ->
    type_args:int ->
    ctors:
      ('a option Tag.t ->
      ([ `Term of string | `Type of string * int ] list
      * Sk.t
      * (app:app -> t option list -> t list -> t list option))
      list) ->
    app:(app:app -> ty_args:t list -> arg:t -> t option) ->
    Sk.t option Tag.t * Sk.t list

  val sexp_of_t : t -> Sexp.t
  val app : app:app -> data -> t -> t option
  val infer : app:app -> t * t option Tag.t -> t Tag.t option
end = struct
  module Known = struct
    type app = t -> t -> t option

    type nonrec t = {
      ctr : int;
      friendly_name : string;
      tag : Sk.t option Tag.t;
      ctors :
        (string list
        * Sk.t
        * (app:app -> t option list -> t list -> t list option))
        list;
      app : app:app -> ty_args:t list -> arg:t -> t option;
    }
  end

  let known = Stack.create ()

  let friendly_sexp_of_data ~sexp_of_t ({ ctr; args } : data) =
    let friendly_name =
      Stack.find_map known ~f:(fun { Known.friendly_name; tag; _ } ->
          if Int.equal tag.ctr ctr then Some friendly_name else None)
      |> Option.value_exn
    in
    if List.is_empty args then Sexp.Atom friendly_name
    else List (Atom friendly_name :: List.map ~f:sexp_of_t args)

  (* like what ppx_sexp_conv generates, but with human-friendly representation for known data types *)
  let rec sexp_of_t t =
    match t with
    | Kty -> Sexp.Atom "Kty"
    | K1ty ty -> List [ Atom "K1ty"; sexp_of_t ty ]
    | Sty -> Sexp.Atom "Sty"
    | S1ty vty -> List [ Atom "S1ty"; sexp_of_t vty ]
    | S2ty (uty, vty) -> List [ Atom "S2ty"; sexp_of_t uty; sexp_of_t vty ]
    | Data data -> friendly_sexp_of_data ~sexp_of_t data

  let tagged f t = tagged f (Tag.sk_of t |> tag)

  let create ~friendly_name ~tag ~ctors ~app =
    let t =
      {
        Known.ctr = tag.Tag.ctr;
        friendly_name;
        tag;
        ctors =
          List.map
            ~f:(fun (vars, ctor, args) ->
              ( List.filter_map
                  ~f:(function `Type _ -> None | `Term var -> Some var)
                  vars,
                ctor,
                args ))
            ctors;
        app;
      }
    in
    Stack.push known t;
    ( tag,
      List.map ctors ~f:(fun (vars, ctor, _) ->
          let tag =
            List.fold_right vars ~init:tag ~f:(fun var tag ->
                match var with
                | `Term _ -> tag
                | `Type (var, index) ->
                    (* specialize tag arg at [index] *)
                    let args =
                      List.mapi tag.Tag.args ~f:(fun i arg ->
                          match (Int.equal i index, arg) with
                          | true, None -> Some (Ref var)
                          | true, Some _ ->
                              failwith "cannot override existing type arg"
                          | false, arg -> arg)
                    in
                    { tag with args })
          in
          List.map ~f:(function `Type (var, _) | `Term var -> var) vars
          |> List.fold_right ~init:(tagged ctor tag) ~f:star) )

  let create_manual ~friendly_name ~type_args ~ctors ~app =
    let args = List.init type_args ~f:(fun _ -> None) in
    let tag = { Tag.ctr = Stack.length known; args } in
    let ctors = ctors tag in
    create ~friendly_name ~tag ~ctors ~app

  let create ~friendly_name ~type_args ~ctors =
    let args = List.init type_args ~f:(fun _ -> None) in
    let tag = { Tag.ctr = Stack.length known; args } in
    let ctors = ctors tag in
    create ~friendly_name ~tag
      ~ctors:
        (List.map
           ~f:(fun (vars, ctor, args) ->
             ( List.map
                 ~f:(function
                   | `Type (var, index) -> `Type (var, index)
                   | `Term (var, _get) -> `Term var)
                 vars,
               ctor,
               args ))
           ctors)
      ~app:(fun ~app ~ty_args ~arg ->
        let from_ctors =
          List.map ctors ~f:(fun (vars, ctor, _args) ->
              let gamma =
                List.map vars ~f:(function
                  | `Type (name, index) -> (name, List.nth_exn ty_args index)
                  | `Term (name, get) -> (name, get ty_args))
              in
              let ty = pc_type gamma ctor in
              app ty arg)
        in
        (* if String.equal friendly_name "nat" then
          List.iter from_ctors ~f:(fun from_ctor ->
              print_s ~mach:() [%sexp (from_ctor : t option)]); *)
        List.reduce_exn from_ctors ~f:(fun a b ->
            match (a, b) with
            | Some a, Some b -> if equal_ty a b then Some a else None
            | _ -> None))

  let find ctr = Stack.find known ~f:(fun { ctr = c; _ } -> Int.equal c ctr)

  let infer ~app (fty, { Tag.ctr; args = tag_args }) =
    let%bind.Option { ctors; _ } = find ctr in
    let%bind.Option args =
      List.find_map ctors ~f:(fun (vars, ctor, to_args) ->
          let%bind.Option res = p_type_match ctor fty in
          let%bind.Option args =
            List.map ~f:(Map.find res) vars |> Option.all
          in
          to_args ~app tag_args args)
    in
    List.iter2_exn
      ~f:(fun a b ->
        Option.iter a ~f:(fun a ->
            if not (equal_ty a b) then failwith "mismatch"))
      tag_args args;
    Some { Tag.ctr; args }

  let app ~app { Tag.ctr; args = ty_args } arg =
    let%bind.Option { app = a; _ } = find ctr in
    a ~app ~ty_args ~arg
end

let sexp_of_t = Data_type.sexp_of_t
