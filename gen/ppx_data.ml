open Ppxlib
open Libsave


type expression = Ppxlib_ast.Ast.expression

type get_data_ret = C : 'a Save.t * 'a -> get_data_ret

let rec get_data expr : get_data_ret =
  match expr with
  | [%expr ()] -> C (Save.Unit, ())
  | [%expr ([%e? a], [%e? b])] ->
    let C (ka, a) = get_data a in
    let C (kb, b) = get_data b in
    C (Save.Seq (ka, kb), (a, b))
  | [%expr Some [%e? a]] ->
    let C (ka, a) = get_data a in
    C (Save.Option ka, Some a)
  | _ ->
    Ppxlib.Location.raise_errorf ~loc:expr.pexp_loc
      "ppx_data: unable to deal with this kind of expression."

let convert_str ~loc str : expression = {
  pexp_desc = Pexp_constant (Pconst_string (str, loc, None)) ;
  pexp_loc = loc ;
  pexp_loc_stack = [] ;
  pexp_attributes = []
}

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
  let C (kind, data) = get_data expr in
  let str = Write.encode kind data in
  [%expr Libsave.Read.decode [%e convert_kind ~loc kind] [%e convert_str ~loc str]]

let expand_data ~loc ~path pattern expr = {
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

