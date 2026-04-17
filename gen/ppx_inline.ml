(* Although the ocamlc compiler is able to inline function without any issue, js_of_ocaml isn't able
 to do it properly, and this causes issues with JavaScript's small stack, especially in the context
 of monadic code.
 To circumvent this issue, this ppx rewriter enables to mark some functions for inlining. *)

open Ppxlib

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

(** Whether a function is local or global. *)
type locality =
  | Local
  | Global

(** The functions to be inlined and their associated expression. *)
let function_map = ref SMap.empty

(** Add the function to the global functions to be inlined. *)
let expand_fun_decl ~loc ~path x expr =
  ignore path ;
  print_endline (Printf.sprintf "Debug: adding %s." x) ;
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
  let o =
    object
      inherit [string list] Ast_traverse.fold as super
      method! pattern p acc =
        let acc = super#pattern p acc in
        match p.ppat_desc with
        | Ppat_var { txt = x ; _ } -> x :: acc
        | _ -> acc
    end in
  fun p ->
    let l = o#pattern p [] in
    List.sort_uniq compare l

(** Return a fresh name starting by its argument. *)
let fresh_name =
  let counter = ref 0 in
  fun n ->
    incr counter ;
    Printf.sprintf "%s_gen_%i" n !counter

(** Given an environment mapping names to other names, replaces all occurences of these. *)
let replace_names, replace_names_pat =
  let o =
    object (self)
      inherit [_] Ast_traverse.map_with_context as super
      method! pattern env p =
        match p.ppat_desc with
        | Ppat_var { txt = x ; loc } ->
          begin match SMap.find_opt x env with
          | None -> p
          | Some x' -> { p with ppat_desc = Ppat_var { txt = x' ; loc } }
          end
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
  o#expression, o#pattern

(** Remove all occurences of a type in an expression, replacing it with an any ([_]) type. *)
let remove_type name =
  object
    inherit Ast_traverse.map as super
    method! core_type_desc = function
      | Ptyp_constr ({ txt = Lident n ; _ }, _)
        when n = name ->
        Ptyp_any
      | t -> super#core_type_desc t
  end#expression

(** Inline the functions given in the environment.
  The environment maps names to pairs of locality and terms. *)
let inline_within =
  let traverse =
    object (self)
      inherit [_] Ast_traverse.map_with_context as super
      method! expression env e =
        let loc = e.pexp_loc in
        let shadow_cases env =
          List.map (fun { pc_lhs ; pc_guard ; pc_rhs } ->
            let names = pattern_names pc_lhs in
            (* We create an environment replacing all local names by new fresh ones. *)
            let env_names =
              List.fold_left (fun e n ->
                SMap.add n (fresh_name n) e) SMap.empty names in
            {
              pc_lhs = replace_names_pat env_names pc_lhs ;
              pc_guard =
                Option.map (fun e -> self#expression env (replace_names env_names e)) pc_guard ;
              pc_rhs = self#expression env (replace_names env_names pc_rhs)
            }) in
        match e.pexp_desc with
        | Pexp_ident { txt = Lident x ; _ }
          when match SMap.find_opt x env with
            | Some (Local, _) -> true
            | _ -> false ->
          let (_locality, e_inlined) = SMap.find x env in
          print_endline (Printf.sprintf "Debug: inlining %s." x) ;
          super#expression env e_inlined
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident x ; _ } ; _ }, args)
          when SMap.mem x env ->
          print_endline (Printf.sprintf "Debug: apply with %s" x) ;
          let rec aux env args e =
            match args, e with
            | args, { pexp_desc = Pexp_ident { txt = Lident x ; _ } } when SMap.mem x env ->
              let (_locality, e_inlined) = SMap.find x env in
              print_endline (Printf.sprintf "Debug: inlining %s." x) ;
              aux env args e_inlined
            | args, { pexp_desc = Pexp_newtype ({ txt = t ; _ }, e_inner) ; _ } ->
              print_endline "Debug: newtype" ;
              aux env args (remove_type t e_inner)
            | args, { pexp_desc = Pexp_constraint (e_inner, _) ; _ } ->
              print_endline "Debug: constraint" ;
              aux env args e_inner
            | (lbl1, arg1) :: args, { pexp_desc =
                Pexp_fun (lbl2, None, { ppat_desc =
                  (Ppat_var { txt = x ; _ }
                  | Ppat_constraint ({ ppat_desc = Ppat_var { txt = x ; _ } }, _)) ; _ }, e_inner) ; _ }
              when lbl1 = lbl2 ->
              print_endline (Printf.sprintf "Debug: adding local %s." x) ;
              let env = SMap.add x (Local, arg1) env in
              aux env args e_inner
            | [], e -> self#expression env e
            | args, e ->
              super#expression env (make_expr ~loc (Pexp_apply (e, args))) in
          let (_locality, e_inlined) = SMap.find x env in
          aux env args e_inlined
        | Pexp_letop {
            let_ = { pbop_op = { txt = op ; _ } ; pbop_pat = p ; pbop_exp = e1 ; _ } ;
            ands = [] ; body = e2 }
          when SMap.mem op env ->
          print_endline "Debug: letop" ;
          self#expression env (make_expr ~loc (Pexp_apply (
            make_expr ~loc (Pexp_ident { txt = Lident op ; loc }),
            [(Nolabel, e1) ; (Nolabel, make_expr ~loc (Pexp_fun (Nolabel, None, p, e2)))])))

        (* Dealing with shadowing. *)
        | Pexp_fun (lbl, eo1, p, e2) ->
          let names = pattern_names p in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let eo1 = Option.map (fun e -> self#expression env (replace_names env_names e)) eo1 in
          let p = replace_names_pat env_names p in
          let e2 = self#expression env (replace_names env_names e2) in
          make_expr ~loc (Pexp_fun (lbl, eo1, p, e2))
        | Pexp_for (p, e1, e2, dir, e3) ->
          let names = pattern_names p in
          let env_names =
            List.fold_left (fun e n ->
              SMap.add n (fresh_name n) e) SMap.empty names in
          let p = replace_names_pat env_names p in
          let e1 = self#expression env e1 in
          let e2 = self#expression env e2 in
          let e3 = self#expression env (replace_names env_names e3) in
          make_expr ~loc (Pexp_for (p, e1, e2, dir, e3))
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
                pbop_exp = self#expression env binding.pbop_exp } in
          let let1 = map_let let1 in
          let lets2 = List.map map_let lets2 in
          let e3 = self#expression env (replace_names env_names e3) in
          make_expr ~loc (Pexp_letop { let_ = let1 ; ands = lets2 ; body = e3 })
        | Pexp_function cases -> make_expr ~loc (Pexp_function (shadow_cases env cases))
        | Pexp_match (e1, cases) ->
          make_expr ~loc (Pexp_match (self#expression env e1, shadow_cases env cases))
        | Pexp_try (e1, cases) ->
          make_expr ~loc (Pexp_try (self#expression env e1, shadow_cases env cases))

        | _ -> super#expression env e
    end in
  traverse#expression

let expand_inline_within ~loc ~path rf pat expr =
  print_endline "Debug: expand_inline_within" ;
  ignore path ; {
    pstr_desc =
      Pstr_value (rf, [{
        pvb_pat = pat ;
        pvb_expr = inline_within !function_map expr ;
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

