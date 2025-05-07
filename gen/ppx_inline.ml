(* Although the ocamlc compiler is able to inline function without any issue, js_of_ocaml isn't able
 to do it properly, and this causes issues with JavaScript's small stack, especially in the context
 of monadic code.
 To circumvent this issue, this is a small ppx rewriter to inline functions. *)

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

(* The functions to be inlined and their associated expression. *)
let function_map = ref SMap.empty


let expand_fun_decl ~loc ~path x expr =
  ignore path ;
  function_map := SMap.add x expr !function_map ; {
    pstr_desc =
      Pstr_value (Nonrecursive, [{
        pvb_pat = make_pat ~loc (Ppat_var { txt = x ; loc }) ;
        pvb_expr = expr ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }

let inline_within =
  let remove_type name =
    object
      inherit Ast_traverse.map as super
      method! core_type_desc = function
        | Ptyp_constr ({ txt = Lident n ; _ }, _)
          when n = name ->
          Ptyp_any
        | t -> super#core_type_desc t
    end#expression in
  let traverse =
    object (self)
      inherit Ast_traverse.map as super
      method! expression e =
        let loc = e.pexp_loc in
        match e.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident x ; _ } ; _ }, args)
          when SMap.mem x !function_map ->
          let e_inlined = SMap.find x !function_map in
          let rec aux args e =
            match args, e with
            | args, { pexp_desc = Pexp_newtype ({ txt = t ; _ }, e_inner) ; _ } ->
              aux args (remove_type t e_inner)
            | args, { pexp_desc = Pexp_constraint (e_inner, _) ; _ } ->
              aux args e_inner
            | [], e -> e
            | (lbl1, arg1) :: args, { pexp_desc = Pexp_fun (lbl2, None, p, e_inner) ; _ }
              when lbl1 = lbl2 ->
              make_expr ~loc (Pexp_let (Nonrecursive, [{ (* This is unfortunately not enough: we still need to unfold until its actual application. *)
                  pvb_pat = p ;
                  pvb_expr = super#expression arg1 ;
                  pvb_attributes = [] ;
                  pvb_loc = loc
                }], aux args e_inner))
            | args, e -> make_expr ~loc (Pexp_apply (e, args)) in
          self#expression (aux args e_inlined)
        | Pexp_letop {
            let_ = { pbop_op = { txt = op ; _ } ; pbop_pat = p ; pbop_exp = e1 ; _ } ;
            ands = [] ; body = e2 }
          when SMap.mem op !function_map ->
          self#expression (make_expr ~loc (Pexp_apply (
            make_expr ~loc (Pexp_ident { txt = Lident op ; loc }),
            [(Nolabel, e1) ; (Nolabel, make_expr ~loc (Pexp_fun (Nolabel, None, p, e2)))])))
        | _ -> super#expression e
    end in
  traverse#expression

let expand_inline_within ~loc ~path rf pat expr =
  ignore path ; {
    pstr_desc =
      Pstr_value (rf, [{
        pvb_pat = pat ;
        pvb_expr = inline_within expr ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }

let fun_decl_extension =
  Extension.declare "inline"
    Extension.Context.Structure_item
    Ast_pattern.(pstr @@ (pstr_value nonrecursive (
      value_binding ~pat:(ppat_var __) ~expr:__
      ^:: nil)) ^:: nil)
    expand_fun_decl

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

