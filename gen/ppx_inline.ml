(** Although the ocamlc compiler is able to inline function without any issue, js_of_ocaml isn't able
 to do it properly, and this causes issues with JavaScript's small stack, especially in the context
 of monadic code.
 To circumvent this issue, this ppx rewriter enables to mark some functions for inlining. *)

(* TODO: also replace [let rec f = fun a b c -> ...] with let rec f a b c = ...] as well as the inner calls. *)

open Ppxlib

(** Print-out an expression, for debugging purposes. *)
let expr_to_string e =
  let buf = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer buf in
  Pprintast.expression fmt e ;
  Format.pp_print_flush fmt () ;
  Buffer.contents buf

let%test "expr_to_string" =
  let loc = Location.none in
  expr_to_string [%expr let (a, b) = (1, 2) in a + b] = "let (a, b) = (1, 2) in a + b"

module SMap = Map.Make (String)

let make_expr ~loc pexp_desc : expression = {
  pexp_desc ;
  pexp_loc = loc ;
  pexp_loc_stack = [] ;
  pexp_attributes = []
}

let make_pat ~loc ppat_desc : Ppxlib_ast.Ast.pattern = {
  ppat_desc ;
  ppat_loc = loc ;
  ppat_loc_stack = [] ;
  ppat_attributes = []
}

let add_debug str e =
  let loc = e.pexp_loc in
  let str = Pconst_string (str, loc, None) in
  let attr = {
    attr_name = { txt = "debug" ; loc } ;
    attr_payload =
      PStr [{
        pstr_loc = loc ;
        pstr_desc = Pstr_eval (make_expr ~loc (Pexp_constant str), [])
      }] ;
    attr_loc = loc
  } in
  { e with pexp_attributes = attr :: e.pexp_attributes }


(** Whether a function is local or global. *)
type locality =
  | Local
  | Global

(** The functions to be inlined and their associated expression. *)
let function_map = ref SMap.empty

(** Mostly for debugging purposes: print the environment. *)
let print_env env =
  String.concat " ; " (List.map (fun (f, _locality) -> f) (SMap.to_list env))

(** Add the function to the global functions to be inlined. *)
let expand_fun_decl ~loc ~path x expr =
  ignore path ;
  function_map := SMap.add x (Global, expr) !function_map ; {
    pstr_desc =
      Pstr_value (Nonrecursive, [{
        pvb_pat = make_pat ~loc (Ppat_var { txt = x ; loc }) ;
        pvb_expr = expr ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }

(** Return all the names within a pattern. *)
let pattern_names =
  let traverse =
    object
      inherit [string list] Ast_traverse.fold as super
      method! pattern p acc =
        let acc = super#pattern p acc in
        match p.ppat_desc with
        | Ppat_var { txt = x ; _ } -> x :: acc
        | Ppat_alias (_, { txt = x ; _ }) -> x :: acc
        | _ -> acc
    end in
  fun p ->
    let l = traverse#pattern p [] in
    List.sort_uniq compare l

let test_pattern_names p names =
  let normalise = List.sort compare in
  normalise (pattern_names p) = normalise names

let%test "pattern_names (a, b, c)" =
  let loc = Location.none in
  test_pattern_names [%pat? (a, b, c)] ["a"; "b"; "c"]
let%test "pattern_names (Either.Left a | Either.Right b)" =
  let loc = Location.none in
  test_pattern_names [%pat? (Either.Left a | Either.Right b)] ["a"; "b"]

(** Return a fresh name starting by its argument. *)
let fresh_name =
  let counter = ref 0 in
  fun n ->
    incr counter ;
    let r = Printf.sprintf "%s_gen_%i" n !counter in
    r

(** Given an environment mapping names to other names, replaces all occurences of these. *)
let (replace_names, replace_names_pat) =
  let traverse =
    object (self)
      inherit [_] Ast_traverse.map_with_context as super
      method! pattern env p =
        match p.ppat_desc with
        | Ppat_var { txt = x ; loc } ->
          begin match SMap.find_opt x env with
          | None -> p
          | Some x' -> { p with ppat_desc = Ppat_var { txt = x' ; loc } }
          end
        | Ppat_alias (p', { txt = x ; loc }) ->
          let p' = self#pattern env p' in
          let x =
            match SMap.find_opt x env with
            | None -> x
            | Some x' -> x' in
          { p with ppat_desc = Ppat_alias (p', { txt = x ; loc }) }
        | _ -> super#pattern env p
      method! expression env e =
        match e.pexp_desc with
        | Pexp_ident { txt = Lident x ; loc } ->
          begin match SMap.find_opt x env with
          | None -> e
          | Some x' -> { e with pexp_desc = Pexp_ident { txt = Lident x' ; loc } }
          end
        | _ -> super#expression env e
    end in
  (traverse#expression, traverse#pattern)

let%test "replace_names" =
  let loc = Location.none in
  let env = SMap.add "a" "e" (SMap.add "c" "f" SMap.empty) in
  replace_names env [%expr let (a, b) = c + d in c + a] = [%expr let (e, b) = f + d in f + e]

(** Remove all occurences of a type in an expression, replacing it with an any ([_]) type. *)
let remove_type name =
  object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_constr ({ txt = Lident n ; _ }, _) when n = name -> Ptyp_any
      | t -> super#core_type_desc t
  end#expression

(** Rewrite an expression to make sure that it only introduces fresh identifiers. *)
let rewrite_fresh =
  let traverse =
    object (self)
      inherit Ast_traverse.map as super
      method! expression e =
        let shadow_cases =
          List.map (fun { pc_lhs ; pc_guard ; pc_rhs } ->
            let names = pattern_names pc_lhs in
            (* We create an environment replacing all local names by new fresh ones. *)
            let env_names =
              List.fold_left (fun e n ->
                SMap.add n (fresh_name n) e) SMap.empty names in
            {
              pc_lhs = replace_names_pat env_names pc_lhs ;
              pc_guard =
                Option.map (fun e -> self#expression (replace_names env_names e)) pc_guard ;
              pc_rhs = self#expression (replace_names env_names pc_rhs)
            }) in
        match e.pexp_desc with
        | Pexp_let (rf, vbs, e1) ->
          let names =
            let l = List.fold_left (fun l vb -> pattern_names vb.pvb_pat @ l) [] vbs in
            List.sort_uniq compare l in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let vbs =
            List.map (fun vb ->
              let expr = vb.pvb_expr in
              let expr =
                match rf with
                | Nonrecursive -> expr
                | Recursive -> replace_names env_names expr in
              let expr = self#expression expr in
              { vb with
                  pvb_pat = replace_names_pat env_names vb.pvb_pat ;
                  pvb_expr = expr }) vbs in
          let e1 = self#expression (replace_names env_names e1) in
          { e with pexp_desc = Pexp_let (rf, vbs, e1) }
        | Pexp_fun (lbl, eo1, p, e2) ->
          let names = pattern_names p in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let eo1 = Option.map (fun e -> self#expression (replace_names env_names e)) eo1 in
          let p = replace_names_pat env_names p in
          let e2 = self#expression (replace_names env_names e2) in
          { e with pexp_desc = Pexp_fun (lbl, eo1, p, e2) }
        | Pexp_for (p, e1, e2, dir, e3) ->
          let names = pattern_names p in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let p = replace_names_pat env_names p in
          let e1 = self#expression e1 in
          let e2 = self#expression e2 in
          let e3 = self#expression (replace_names env_names e3) in
          { e with pexp_desc = Pexp_for (p, e1, e2, dir, e3) }
        | Pexp_letop { let_ = let1 ; ands = lets2 ; body = e3 } ->
          let names =
            let l = List.fold_left (fun l bo -> pattern_names bo.pbop_pat @ l) [] (let1 :: lets2) in
            List.sort_uniq compare l in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let map_let binding =
            { binding with
                pbop_pat = replace_names_pat env_names binding.pbop_pat ;
                pbop_exp = self#expression binding.pbop_exp } in
          let let1 = map_let let1 in
          let lets2 = List.map map_let lets2 in
          let e3 = self#expression (replace_names env_names e3) in
          { e with pexp_desc = Pexp_letop { let_ = let1 ; ands = lets2 ; body = e3 } }
        | Pexp_function cases -> { e with pexp_desc = Pexp_function (shadow_cases cases) }
        | Pexp_match (e1, cases) ->
          { e with pexp_desc = Pexp_match (self#expression e1, shadow_cases cases) }
        | Pexp_try (e1, cases) ->
          { e with pexp_desc = Pexp_try (self#expression e1, shadow_cases cases) }

        | _ -> super#expression e
    end in
  traverse#expression

(** Inline the functions given in the environment.
  The environment maps names to pairs of locality and terms. *)
let inline_within =
  let traverse =
    object (self)
      inherit [_] Ast_traverse.map_with_context as super
      method! expression env e =
        let loc = e.pexp_loc in
        match e.pexp_desc with
        | Pexp_ident { txt = Lident x ; _ }
            when match SMap.find_opt x env with
              | Some (Local, _) -> true
              | _ -> false ->
          let (_locality, e_inlined) = SMap.find x env in
          let e_inlined = rewrite_fresh e_inlined in
          super#expression env e_inlined
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident x ; _ } ; _ }, args)
            when SMap.mem x env ->
          let (_locality, e_inlined) = SMap.find x env in
          let e_inlined = rewrite_fresh e_inlined in
          let rec aux env args e =
            match args, e with
            | args, { pexp_desc = Pexp_ident { txt = Lident x ; _ } ; _ } when SMap.mem x env ->
              let (_locality, e_inlined) = SMap.find x env in
              let e_inlined = rewrite_fresh e_inlined in
              aux env args e_inlined
            | [], e -> self#expression env e
            | args, { pexp_desc = Pexp_newtype ({ txt = t ; _ }, e_inner) ; _ } ->
              aux env args (remove_type t e_inner)
            | args, { pexp_desc = Pexp_constraint (e_inner, ty) ; _ } ->
              (* We need to dispatch the types to the additional arguments. *)
              let (args, ty') =
                let rec aux acc args ty =
                  match args, ty with
                  | [], ty -> (List.rev acc, ty)
                  | (lbl1, arg) :: args', { ptyp_desc = Ptyp_arrow (lbl2, ty_arg, ty') ; _ }
                    when lbl1 = lbl2 ->
                    aux ((lbl1, make_expr ~loc (Pexp_constraint (arg, ty_arg))) :: acc) args' ty'
                  | _, _ -> (List.rev_append acc args, [%type: _]) in
                aux [] args ty in
              make_expr ~loc (Pexp_constraint (aux env args e_inner, ty'))
            | (lbl1, arg1) :: args, { pexp_desc =
                Pexp_fun (lbl2, None, { ppat_desc =
                  (Ppat_var { txt = x ; _ }
                  | Ppat_constraint ({ ppat_desc = Ppat_var { txt = x ; _ } ; _ }, _))
                  ; _ }, e_inner) ; _ }
                when lbl1 = lbl2 ->
              let x' = fresh_name x in
              let env_name = SMap.add x x' SMap.empty in
              let e_inner = replace_names env_name e_inner in
              let env = SMap.add x' (Local, arg1) env in
              aux env args e_inner
            | args, { pexp_desc = Pexp_apply (e, args') ; _ } -> aux env (args' @ args) e
            | args, { pexp_desc = Pexp_let (rf, vs, e) ; _ } ->
              let vs = List.map (super#value_binding env) vs in
              make_expr ~loc (Pexp_let (rf, vs, aux env args e))
            | args, e -> super#expression env (make_expr ~loc (Pexp_apply (e, args))) in
          aux env args e_inlined
        | Pexp_letop {
            let_ = { pbop_op = { txt = op ; loc } ; pbop_pat = p ; pbop_exp = e1 ; _ } ;
            ands = [] ; body = e2 }
            when SMap.mem op env ->
          let names = pattern_names p in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let p = replace_names_pat env_names p in
          let e2 = replace_names env_names e2 in
          self#expression env (make_expr ~loc (Pexp_apply (
            make_expr ~loc (Pexp_ident { txt = Lident op ; loc }),
            [(Nolabel, e1) ; (Nolabel, make_expr ~loc (Pexp_fun (Nolabel, None, p, e2)))])))
        | _ -> super#expression env e
    end in
  traverse#expression

(** Whether an expression may perform side effects. *)
let side_effect =
  let traverse =
    object (self)
      inherit [_] Ast_traverse.lift
      method! expression e =
        let aux_option = function
          | None -> false
          | Some e -> self#expression e in
        match e.pexp_desc with
        | Pexp_ident _
        | Pexp_constant _ -> false
        | Pexp_let (_rf, vbs, e) ->
          self#expression e
          || List.exists (fun (vb : value_binding) ->
               self#expression vb.pvb_expr) vbs
        | Pexp_function cases ->
          List.exists (fun case ->
            aux_option case.pc_guard
            || self#expression case.pc_rhs) cases
        | Pexp_fun (_, eo, _, e) ->
          aux_option eo || self#expression e
        | Pexp_apply (e, es) ->
          self#expression e
          || List.exists (fun (_, e) -> self#expression e) es
        | Pexp_match (e, cases) ->
          self#expression e
          || List.exists (fun case ->
               aux_option case.pc_guard
               || self#expression case.pc_rhs) cases
        | Pexp_tuple es -> List.exists self#expression es
        | Pexp_construct (_, eo)
        | Pexp_variant (_, eo) -> aux_option eo
        | Pexp_record (fes, eo) ->
          aux_option eo
          || List.exists (fun (_f, e) -> self#expression e) fes
        | Pexp_field (e, _) -> self#expression e
        | Pexp_array es -> List.exists self#expression es
        | Pexp_ifthenelse (b, e1, eo2) ->
          self#expression b
          || self#expression e1
          || aux_option eo2
        | Pexp_constraint (e, _) -> self#expression e
        | Pexp_newtype (_, e) -> self#expression e
        | Pexp_open (_, e) -> self#expression e
        | _ -> true
      method other _ = true
      method int _ = false
      method string _ = false
      method bool _ = false
      method char _ = false
      method array = Array.exists
      method float _ = false
      method int32 _ = false
      method int64 _ = false
      method nativeint _ = false
      method unit _ = false
      method record = List.exists snd
      method tuple = List.exists (fun b -> b)
      method constr _ = self#tuple
    end in
  traverse#expression

(** Due to the way some monads are built, inlining them often lead to unnecessarily deep
  function abstractions. This can confuse js_of_ocaml. This function thus tries to move
  them as far as possible to the outside, as is more usual. *)
let move_abstractions_up expr = (*
  let traverse =
    object (self)
      inherit [_] Ast_traverse.lift as super
      method! expression e =
        let loc = e.pexp_loc in
        match e.pexp_desc with
        | Pexp_ident _ -> ([], e)
        | 
        | _ ->
          let (_args, e) = super#expression e in
          ([], e)
    end in
  snd (traverse#expression expr) *) expr

let expand_inline_within ~loc ~path rf pat expr =
  ignore path ; {
    pstr_desc =
      Pstr_value (rf, [{
        pvb_pat = pat ;
        pvb_expr = move_abstractions_up (inline_within !function_map expr) ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }

(** Mark this function to be inlined. *)
let fun_decl_extension =
  Extension.declare "inline"
    Extension.Context.Structure_item
    Ast_pattern.(pstr @@ (pstr_value nonrecursive (
      value_binding ~pat:(ppat_var __) ~expr:__
      ^:: nil)) ^:: nil)
    expand_fun_decl

(** Inline the above-marked functions in this code. *)
let inline_within_extension =
  Extension.declare "inline_within"
    Extension.Context.Structure_item
    Ast_pattern.(pstr @@ (pstr_value __ (
      value_binding ~pat:__ ~expr:__
      ^:: nil)) ^:: nil)
    expand_inline_within

let fun_decl_rule = Context_free.Rule.extension fun_decl_extension
let inline_within_rule = Context_free.Rule.extension inline_within_extension

let () =
  Driver.register_transformation ~rules:[fun_decl_rule; inline_within_rule] "ppx_inline"

