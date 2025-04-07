open Ppxlib
open Libsave


type expression = Ppxlib_ast.Ast.expression

let make_exp ~loc pexp_desc : expression = {
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

(* The function [get_data] returns a triple of a save kind, the corresponding data,
  and a function expression to convert this data into what is actually expected at this
  place of the program. *)
type get_data_ret = C : 'a Save.t * 'a * expression -> get_data_ret

let rec get_data expr : get_data_ret =
  let loc = expr.pexp_loc in
  match expr with

  (* Tuples *)
  | [%expr ()] -> C (Save.Unit, (), [%expr fun _ -> ()])
  | { pexp_desc = Pexp_tuple l ; _ } ->
    let C (k, a, convert) =
      List.fold_left (fun da b ->
        let C (ka, a, converta) = da in
        let C (kb, b, convertb) = get_data b in
        C (Save.Seq (ka, kb), (a, b), [%expr fun (a, b) -> ([%e converta] a, [%e convertb] b)])
      ) (get_data [%expr ()]) l in
    let ids = List.mapi (fun i _ -> Printf.sprintf "x%i" i) l in
    let lid = List.map (fun id -> make_exp ~loc (Pexp_ident { txt = Lident id ; loc })) ids in
    let cid =
      List.fold_left (fun p id ->
        [%pat? ([%p p], [%p make_pat ~loc (Ppat_var { txt = id ; loc })])]) [%pat? ()] ids in
    C (k, a, [%expr fun l -> let [%p cid] = [%e convert] l in [%e make_exp ~loc (Pexp_tuple lid)]])

  (* Constants *)
  | { pexp_desc = Pexp_constant (Pconst_integer (str, None)) ; _ } ->
    C (Save.Int, int_of_string str, [%expr fun i -> i])
  | { pexp_desc = Pexp_constant (Pconst_string (str, _, _)) ; _ } ->
    C (Save.String, str, [%expr fun str -> str])
  | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None) ; _ } ->
    C (Save.Bool, true, [%expr fun b -> b])
  | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None) ; _ } ->
    C (Save.Bool, false, [%expr fun b -> b])

  | [%expr Some [%e? a]] ->
    let C (ka, a, convert) = get_data a in
    C (Save.Option ka, Some a, [%expr Option.map [%e convert]])

  | _ ->
    Ppxlib.Location.raise_errorf ~loc:expr.pexp_loc
      "ppx_data: unable to deal with this kind of expression."

let convert_str ~loc str : expression =
  make_exp ~loc (Pexp_constant (Pconst_string (str, loc, None)))

let rec convert_kind : type a. loc:_ -> a Save.t -> expression = fun ~loc -> function
  | Save.Unit -> [%expr Libsave.Save.Unit ]
  | Save.Bool -> [%expr Libsave.Save.Bool ]
  | Save.Int -> [%expr Libsave.Save.Int ]
  | Save.String -> [%expr Libsave.Save.String ]
  | Save.Seq (a, b) ->
    [%expr Libsave.Save.Seq ([%e convert_kind ~loc a], [%e convert_kind ~loc b])]
  | Save.Option a -> [%expr Libsave.Save.Option [%e convert_kind ~loc a]]
  | Save.List a -> [%expr Libsave.Save.List [%e convert_kind ~loc a]]
  | Save.Array a -> [%expr Libsave.Save.Array [%e convert_kind ~loc a]]
  | Save.AddColor a -> [%expr Libsave.Save.AddColor [%e convert_kind ~loc a]]
  | Save.Image -> [%expr Libsave.Save.Image]
  | Save.Base64 a -> [%expr Libsave.Save.Base64 [%e convert_kind ~loc a]]
  | Save.Compress a -> [%expr Libsave.Save.Compress [%e convert_kind ~loc a]]

let encode_data expr =
  let loc = expr.pexp_loc in
  let C (kind, data, decode) = get_data expr in
  let kind = Save.Base64 (Save.Compress kind) in
  let str = Write.encode kind data in
  let e =
    [%expr Libsave.Read.decode [%e convert_kind ~loc kind] [%e convert_str ~loc str]] in
  make_exp ~loc (Pexp_apply (decode, [(Nolabel, e)]))

let expand_data ~loc ~path pattern expr =
  ignore path ; {
    pstr_desc =
      Pstr_value (Nonrecursive, [{
        pvb_pat = pattern ;
        pvb_expr = encode_data expr ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }

let extension =
  Extension.declare "data"
    Extension.Context.Structure_item
    Ast_pattern.(pstr @@ (pstr_value drop ((value_binding ~pat:__ ~expr:__) ^:: nil)) ^:: nil)
    expand_data

let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation ~rules:[rule] "ppx_data"

