open Ppxlib
open Libsave


(* * Raw Data *)

type expression = Ppxlib_ast.Ast.expression

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

(* The function [get_data] returns a triple of a save kind, the corresponding data,
  and a function expression to convert this data into what is actually expected at this
  place of the program. *)
type get_data_ret = C : ('a Save.t * expression) option * 'a -> get_data_ret

let rec get_data expr : get_data_ret =
  let loc = expr.pexp_loc in
  match expr with

  (* Tuples *)
  | [%expr ()] -> C (Some (Save.Unit, [%expr fun _ -> ()]), ())
  | { pexp_desc = Pexp_tuple l ; _ } ->
    let C (k, a) =
      List.fold_left (fun da b ->
        let C (ka, a) = da in
        let C (kb, b) = get_data b in
        let k =
          Option.bind ka (fun (ka, converta) ->
            Option.bind kb (fun (kb, convertb) ->
              Some (Save.Seq (ka, kb), [%expr fun (a, b) -> ([%e converta] a, [%e convertb] b)]))) in
        C (k, (a, b))
      ) (get_data [%expr ()]) l in
    let k =
      Option.map (fun (k, convert) ->
        let ids = List.mapi (fun i _ -> Printf.sprintf "x%i" i) l in
        let lid = List.map (fun id -> make_expr ~loc (Pexp_ident { txt = Lident id ; loc })) ids in
        let cid =
          List.fold_left (fun p id ->
            [%pat? ([%p p], [%p make_pat ~loc (Ppat_var { txt = id ; loc })])]) [%pat? ()] ids in
        (k, [%expr fun c -> let [%p cid] = [%e convert] c in [%e make_expr ~loc (Pexp_tuple lid)]])) k in
    C (k, a)

  (* Constants *)
  | { pexp_desc = Pexp_constant (Pconst_integer (str, None)) ; _ } ->
    C (Some (Save.Int, [%expr fun i -> i]), int_of_string str)
  | { pexp_desc = Pexp_constant (Pconst_string (str, _, _)) ; _ } ->
    C (Some (Save.String, [%expr fun str -> str]), str)
  | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None) ; _ } ->
    C (Some (Save.Bool, [%expr fun b -> b]), true)
  | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None) ; _ } ->
    C (Some (Save.Bool, [%expr fun b -> b]), false)

  (* Option, lists, and arrays *)
  | [%expr None] -> C (None, None)
  | [%expr Some [%e? a]] ->
    let C (k, a) = get_data a in
    C (Option.map (fun (k, convert) -> (Save.Option k, [%expr Option.map [%e convert]])) k, Some a)
  | [%expr []] -> C (None, [])
  | [%expr [%e? x] :: [%e? l]] ->
    let C (kx, x) = get_data x in
    let C (kl, l) = get_data l in
    (* We know that the type-checker will fail reject the program if kl isn't a list of kx, so we
     just use Obj.magic, knowing that the type-checker will actually check this. *)
    let k =
      match kl with
      | None ->
        Option.map (fun (k, convert) ->
          (Save.List k, [%expr List.map [%e convert]])) kx
      | Some _ -> Obj.magic kl in
    C (k, x :: Obj.magic l)
  | { pexp_desc = Pexp_array l ; _ } ->
    let (k, l) =
      List.fold_left (fun (k, acc) x ->
        let C (kx, x) = get_data x in
        (* Again, we know thanks to the type-checker that all the elements of the array have the
          same type. *)
        let k =
          match k with
          | Some _ -> k
          | None -> Obj.magic kx in
        (k, Obj.magic x :: acc)
      ) (None, []) l in
    C (Option.map (fun (k, convert) ->
        (Save.Array k, [%expr Array.map [%e convert]])) k, Array.of_list (List.rev l))

  | _ ->
    Ppxlib.Location.raise_errorf ~loc "ppx_data: unable to deal with this kind of expression."

let convert_str ~loc str : expression =
  make_expr ~loc (Pexp_constant (Pconst_string (str, loc, None)))

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

let encode_data ~loc data =
  let C (kind, data) = data in
  let (kind, decode) =
    match kind with
    | Some (kind, decode) -> (kind, decode)
    | None -> Ppxlib.Location.raise_errorf ~loc "ppx_data: Unable to determine the type of data." in
  let kind = Save.Base64 (Save.Compress kind) in
  let str = Write.encode kind data in
  let e =
    [%expr Libsave.Read.decode [%e convert_kind ~loc kind] [%e convert_str ~loc str]] in
  make_expr ~loc (Pexp_apply (decode, [(Nolabel, e)]))

let expand_data ~loc ~path pattern expr =
  ignore path ; {
    pstr_desc =
      Pstr_value (Nonrecursive, [{
        pvb_pat = pattern ;
        pvb_expr = encode_data ~loc:expr.pexp_loc (get_data expr) ;
        pvb_attributes = [] ;
        pvb_loc = loc
      }]) ;
    pstr_loc = loc
  }


(* * Images *)

let open_image file =
  let channel = open_in_bin file in
  let len = in_channel_length channel in
  let content = really_input_string channel len in
  let chunk = ImageUtil.chunk_reader_of_string content in
  ImageLib.PNG.parsefile chunk

let image_folder = "./src/" (* LATER: images/ *)

let get_colors image =
  let module S = Set.Make (struct type t = Save.color let compare = compare end) in
  let m = ref S.empty in
  for x = 0 to image.Image.width - 1 do
    for y = 0 to image.Image.height - 1 do
      Image.read_rgba image x y (fun r g b a ->
        if a <> 0 then m := S.add (r, g, b, a) !m)
    done
  done ;
  S.to_list !m

let expand_image ~loc ~path str =
  ignore path ;
  let data =
    let img = open_image (image_folder ^ str) in
    let colors = get_colors img in
    List.fold_left (fun d c ->
      let C (d, img) = d in
      let (d, convert) =
        match d with
        | Some (d, convert) -> (d, convert)
        | None -> assert false in
      C (Some (Save.AddColor d, [%expr fun d -> [%e convert] (snd d)]), (c, img))
    ) (C (Some (Save.Image, [%expr fun img -> img]), img)) colors in
  encode_data ~loc data


(* * Rule Declarations *)

let data_extension =
  Extension.declare "data"
    Extension.Context.Structure_item
    Ast_pattern.(pstr @@ (pstr_value drop ((value_binding ~pat:__ ~expr:__) ^:: nil)) ^:: nil)
    expand_data

let image_extension =
  Extension.declare "data_image"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_image

let data_rule = Context_free.Rule.extension data_extension
let image_rule = Context_free.Rule.extension image_extension

let () =
  Driver.register_transformation ~rules:[data_rule; image_rule] "ppx_data"

